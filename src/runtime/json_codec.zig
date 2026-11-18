const std = @import("std");
const v = @import("value.zig");
const VM = @import("vm.zig").VM;
const serialization_plan = @import("serialization_plan.zig");
const native = @import("builtins/native.zig");

pub const JsonError = enum {
    InvalidJson,
    TypeMismatch,
    MissingField,
    UnknownField,
    DuplicateKey,
    NumberOutOfRange,
    UnknownVariant,
    InvalidUnion,
    NonFiniteNumber,
    DepthExceeded,
    CycleDetected,
    InvalidOptions,
    InvalidValue,
};

pub fn encode(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const options = v.HeapValue.getParentPtr(v.Struct, args[1].heap_obj);
    const max_depth = options.get(0).int64;
    if (!isValidDepth(max_depth)) {
        return native.fail(JsonError.InvalidOptions);
    }
    var output: std.Io.Writer.Allocating = .init(allocator);
    var encoder = Encoder{
        .vm = vm,
        .allocator = allocator,
        .writer = .{ .writer = &output.writer, .options = .{} },
        .max_depth = @intCast(max_depth),
        .fragments = v.HeapValue.getParentPtr(v.Array, args[3].heap_obj.followRelocated()),
    };
    encoder.encodeValue(@intCast(args[2].int64), args[0], 0) catch |failure| {
        const is_allocation_failure = failure == error.OutOfMemory or failure == error.WriteFailed;
        if (is_allocation_failure) {
            return error.OutOfMemory;
        }
        return native.fail(std.meta.stringToEnum(JsonError, @errorName(failure)) orelse .InvalidJson);
    };
    return native.ok(try vm.allocBytesValue(output.written(), .string));
}

fn isValidDepth(depth: i64) bool {
    return depth > 0 and depth <= 512;
}

const Encoder = struct {
    vm: *VM,
    allocator: std.mem.Allocator,
    writer: std.json.Stringify,
    max_depth: usize,
    fragments: *v.Array,
    next_fragment: usize = 0,
    ancestors: std.ArrayList(*v.HeapValue) = .empty,

    fn encodeValue(self: *Encoder, plan_index: u32, value: v.Value, depth: usize) anyerror!void {
        const plan = self.vm.program.serialization_plans[plan_index];
        if (plan.shape == .nullable) {
            if (@as(u64, @bitCast(value)) == 0) {
                try self.writer.write(null);
                return;
            }
            const inner = self.vm.program.serialization_plans[plan.shape.nullable];
            const unwrapped = if (inner.kind.isHeapType()) value else v.HeapValue.getParentPtr(v.NullBox, value.heap_obj).value;
            return self.encodeValue(plan.shape.nullable, unwrapped, depth);
        }
        const is_container = switch (plan.shape) {
            .array, .map, .structure, .tagged_union => true,
            else => false,
        };
        if (is_container) {
            if (depth >= self.max_depth) {
                return error.DepthExceeded;
            }
            const object = value.heap_obj.followRelocated();
            for (self.ancestors.items) |ancestor| {
                if (ancestor == object) {
                    return error.CycleDetected;
                }
            }
            try self.ancestors.append(self.allocator, object);
        }
        defer if (is_container) {
            _ = self.ancestors.pop();
        };
        switch (plan.shape) {
            .int, .byte => try self.writer.write(value.int64),
            .float => {
                if (!std.math.isFinite(value.float64)) {
                    return error.NonFiniteNumber;
                }
                try self.writer.write(value.float64);
            },
            .boolean => try self.writer.write(value.bool),
            .string => try self.writeString(bytes(value)),
            .enumeration => |variants| {
                for (variants) |variant| {
                    if (variant.value != value.int64) continue;
                    try self.writer.write(variant.name);
                    return;
                }
                return error.UnknownVariant;
            },
            .structure => |structure| {
                const object = v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
                try self.writer.beginObject();
                for (structure.fields) |field| {
                    const item = object.get(field.slot);
                    const is_omitted = field.omits_null and @as(u64, @bitCast(item)) == 0;
                    if (is_omitted) continue;
                    try self.writer.objectField(field.name);
                    if (field.proxy) |site| {
                        try self.encodeSite(site, item);
                        continue;
                    }
                    try self.encodeValue(field.plan, item, depth + 1);
                }
                try self.writer.endObject();
            },
            .array => |items| {
                const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj.followRelocated());
                try self.writer.beginArray();
                for (array.getDataPtr()[0..array.len]) |item| {
                    if (items.proxy) |site| {
                        try self.encodeSite(site, item);
                        continue;
                    }
                    try self.encodeValue(items.item, item, depth + 1);
                }
                try self.writer.endArray();
            },
            .map => |items| {
                const map = v.HeapValue.getParentPtr(v.Map, value.heap_obj);
                try self.writer.beginObject();
                if (map.getStorage()) |storage| {
                    for (storage.entries()[0..storage.len]) |entry| {
                        if (entry.isRemoved()) continue;
                        const key = bytes(entry.key);
                        if (!std.unicode.utf8ValidateSlice(key)) {
                            return error.InvalidJson;
                        }
                        try self.writer.objectField(key);
                        if (items.proxy) |site| {
                            try self.encodeSite(site, entry.value);
                            continue;
                        }
                        try self.encodeValue(items.item, entry.value, depth + 1);
                    }
                }
                try self.writer.endObject();
            },
            .tagged_union => |tagged| {
                const object = v.HeapValue.getParentPtr(v.Union, value.heap_obj);
                for (tagged.variants) |variant| {
                    if (object.active_tag != variant.tag) continue;
                    try self.writer.beginObject();
                    try self.writer.objectField(variant.name);
                    try self.encodeValue(variant.plan, object.get(variant.tag), depth + 1);
                    try self.writer.endObject();
                    return;
                }
                return error.InvalidUnion;
            },
            .nullable, .proxy => unreachable,
        }
    }

    fn encodeSite(self: *Encoder, site: serialization_plan.ProxySite, value: v.Value) !void {
        const is_null = site.is_nullable_target and @as(u64, @bitCast(value)) == 0;
        if (is_null) {
            try self.writer.write(null);
            return;
        }
        if (self.next_fragment >= self.fragments.len) {
            return error.InvalidJson;
        }
        const fragment = bytes(self.fragments.getDataPtr()[self.next_fragment]);
        self.next_fragment += 1;
        try self.writer.beginWriteRaw();
        try self.writer.writer.writeAll(fragment);
        self.writer.endWriteRaw();
    }

    fn writeString(self: *Encoder, text: []const u8) !void {
        if (!std.unicode.utf8ValidateSlice(text)) {
            return error.InvalidJson;
        }
        try self.writer.write(text);
    }
};

fn bytes(value: v.Value) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, value.heap_obj).slice();
}

pub fn decode(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const root_base = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_base);
    const diagnostic = try vm.heap.protectValue(args[1], .nullable);
    const steps = try vm.heap.protectValue(args[4], .array);
    const source = try allocator.dupe(u8, bytes(args[0]));
    const options = v.HeapValue.getParentPtr(v.Struct, args[2].heap_obj);
    const max_depth = options.get(1).int64;
    var decoder = Decoder{
        .vm = vm,
        .allocator = allocator,
        .source = source,
        .scanner = std.json.Scanner.initCompleteInput(allocator, source),
        .diagnostic = diagnostic,
        .max_depth = if (isValidDepth(max_depth)) @intCast(max_depth) else 0,
        .should_reject_unknown_fields = options.get(0).bool,
        .steps = steps,
    };
    defer decoder.scanner.deinit();
    try decoder.resetDiagnostic();
    const result = decoder.decodeDocument(@intCast(args[3].int64)) catch |failure| {
        if (failure == error.OutOfMemory) {
            return failure;
        }
        try decoder.writeDiagnostic();
        return native.fail(std.meta.stringToEnum(JsonError, @errorName(failure)) orelse .InvalidJson);
    };
    return native.ok(result);
}

const PendingItem = struct {
    index: usize,
    proxy_value: usize,
};

const Decoder = struct {
    vm: *VM,
    allocator: std.mem.Allocator,
    source: []const u8,
    scanner: std.json.Scanner,
    diagnostic: usize,
    max_depth: usize,
    should_reject_unknown_fields: bool,
    offset: usize = 0,
    path: std.ArrayList(u8) = .empty,
    steps: usize,

    fn decodeDocument(self: *Decoder, plan: u32) !v.Value {
        if (self.max_depth == 0) {
            return error.InvalidOptions;
        }
        const value = try self.decodeValue(plan, 0);
        if (try self.peekToken() != .end_of_document) {
            return error.InvalidJson;
        }
        _ = try self.nextToken();
        return self.vm.heap.protectedValue(value);
    }

    fn decodeValue(self: *Decoder, plan_index: u32, depth: usize) anyerror!usize {
        const plan = self.vm.program.serialization_plans[plan_index];
        if (plan.shape == .nullable) {
            if (try self.peekToken() == .null) {
                _ = try self.nextToken();
                return self.vm.heap.protectValue(v.NULL_VALUE, .nullable);
            }
            const child = try self.decodeValue(plan.shape.nullable, depth);
            const inner = self.vm.program.serialization_plans[plan.shape.nullable];
            if (inner.kind.isHeapType()) {
                return child;
            }
            const boxed = try self.vm.allocNullBox(self.vm.heap.protectedValue(child), inner.kind);
            return self.vm.heap.protectValue(boxed, .nullable);
        }
        switch (plan.shape) {
            .int, .byte => {
                const text = try self.readNumberText();
                if (std.mem.indexOfAny(u8, text, ".eE") != null) {
                    return error.TypeMismatch;
                }
                const number = std.fmt.parseInt(i64, text, 10) catch return error.NumberOutOfRange;
                const is_byte_out_of_range = plan.shape == .byte and (number < 0 or number > 255);
                if (is_byte_out_of_range) {
                    return error.NumberOutOfRange;
                }
                return self.vm.heap.protectValue(.{ .int64 = number }, .int64);
            },
            .float => {
                const text = try self.readNumberText();
                const number = std.fmt.parseFloat(f64, text) catch return error.NumberOutOfRange;
                if (!std.math.isFinite(number)) {
                    return error.NumberOutOfRange;
                }
                return self.vm.heap.protectValue(.{ .float64 = number }, .float64);
            },
            .boolean => {
                const item = try self.nextToken();
                const is_true = switch (item) {
                    .true => true,
                    .false => false,
                    else => return error.TypeMismatch,
                };
                return self.vm.heap.protectValue(.{ .bool = is_true }, .bool);
            },
            .string => {
                const text = try self.readStringText();
                const value = try self.vm.allocBytesValue(text, .string);
                return self.vm.heap.protectValue(value, .string);
            },
            .enumeration => |variants| {
                const text = try self.readStringText();
                for (variants) |variant| {
                    if (!variant.isNamed(text)) continue;
                    return self.vm.heap.protectValue(.{ .int64 = variant.value }, .int64);
                }
                return error.UnknownVariant;
            },
            .array => |items| {
                try self.enterContainer(.array_begin, depth);
                const item_kind = self.vm.program.serialization_plans[items.item].kind;
                const array = try self.allocRootedArray(item_kind);
                var pending: std.ArrayList(PendingItem) = .empty;
                var i: usize = 0;
                while (try self.peekToken() != .array_end) : (i += 1) {
                    const path_base = self.path.items.len;
                    try self.appendIndexPath(i);
                    if (items.proxy) |site| {
                        const placeholder = try self.vm.heap.protectValue(v.NULL_VALUE, item_kind);
                        if (try self.decodeSite(site, depth + 1)) |proxy_value| {
                            try pending.append(self.allocator, .{ .index = i, .proxy_value = proxy_value });
                        }
                        try self.appendArrayItem(array, placeholder);
                    } else {
                        const item = try self.decodeValue(items.item, depth + 1);
                        try self.appendArrayItem(array, item);
                    }
                    self.path.shrinkRetainingCapacity(path_base);
                }
                _ = try self.nextToken();
                if (items.proxy) |site| {
                    for (pending.items) |entry| {
                        const index = try self.vm.heap.protectValue(.{ .int64 = @intCast(entry.index) }, .int64);
                        try self.queueStep(site.convert_function, &.{ array, index, entry.proxy_value });
                    }
                }
                return array;
            },
            .structure => |shape| return self.readStruct(shape, depth),
            .map => |items| return self.readMap(items, depth),
            .tagged_union => |shape| return self.readUnion(shape, depth),
            .nullable, .proxy => unreachable,
        }
    }

    fn readNumberText(self: *Decoder) ![]const u8 {
        return switch (try self.nextToken()) {
            .number, .allocated_number => |text| text,
            else => error.TypeMismatch,
        };
    }

    fn readStringText(self: *Decoder) ![]const u8 {
        return switch (try self.nextToken()) {
            .string, .allocated_string => |text| text,
            else => error.TypeMismatch,
        };
    }

    fn enterContainer(self: *Decoder, expected: std.meta.Tag(std.json.Token), depth: usize) !void {
        if (std.meta.activeTag(try self.nextToken()) != expected) {
            return error.TypeMismatch;
        }
        if (depth >= self.max_depth) {
            return error.DepthExceeded;
        }
    }

    fn readStruct(self: *Decoder, shape: serialization_plan.Structure, depth: usize) !usize {
        try self.enterContainer(.object_begin, depth);
        const object = try self.allocRootedStruct(shape.descriptor);
        const seen_fields = try self.allocator.alloc(bool, shape.fields.len);
        @memset(seen_fields, false);
        var keys = std.StringHashMap(void).init(self.allocator);
        while (try self.peekToken() != .object_end) {
            const key = try self.readObjectKey(&keys);
            const path_base = self.path.items.len;
            try self.appendFieldPath(key, false);
            const field_index = for (shape.fields, 0..) |field, i| {
                if (field.isNamed(key)) {
                    break i;
                }
            } else null;
            if (field_index) |i| {
                if (seen_fields[i]) {
                    return error.DuplicateKey;
                }
                seen_fields[i] = true;
                if (shape.fields[i].proxy) |site| {
                    if (try self.decodeSite(site, depth + 1)) |proxy_value| {
                        try self.queueStep(site.convert_function, &.{ object, proxy_value });
                    }
                } else {
                    const item = try self.decodeValue(shape.fields[i].plan, depth + 1);
                    self.rootedStruct(object).set(shape.fields[i].slot, self.vm.heap.protectedValue(item));
                }
            } else {
                if (self.should_reject_unknown_fields) {
                    return error.UnknownField;
                }
                try self.skipValue(depth + 1);
            }
            self.path.shrinkRetainingCapacity(path_base);
        }
        _ = try self.nextToken();
        for (shape.skipped) |skip| {
            try self.queueStep(skip.default_function, &.{object});
        }
        for (shape.fields, 0..) |field, i| {
            if (seen_fields[i]) continue;
            if (field.default_function) |function| {
                try self.queueStep(function, &.{object});
                continue;
            }
            try self.appendFieldPath(field.name, false);
            return error.MissingField;
        }
        return object;
    }

    fn readMap(self: *Decoder, items: serialization_plan.Items, depth: usize) !usize {
        try self.enterContainer(.object_begin, depth);
        const value_kind = self.vm.program.serialization_plans[items.item].kind;
        const value = try self.vm.newMap(.string, value_kind);
        const map = try self.vm.heap.protectValue(value, .map);
        var keys = std.StringHashMap(void).init(self.allocator);
        while (try self.peekToken() != .object_end) {
            const key_text = try self.readObjectKey(&keys);
            const path_base = self.path.items.len;
            try self.appendFieldPath(key_text, true);
            const key_value = try self.vm.allocBytesValue(key_text, .string);
            const key_root = try self.vm.heap.protectValue(key_value, .string);
            if (items.proxy) |site| {
                const placeholder = try self.vm.heap.protectValue(v.NULL_VALUE, value_kind);
                const proxy_value = try self.decodeSite(site, depth + 1);
                try self.vm.setMapEntry(&self.vm.heap.temporary_roots.items[map].value, &self.vm.heap.temporary_roots.items[key_root].value, &self.vm.heap.temporary_roots.items[placeholder].value);
                if (proxy_value) |found| {
                    try self.queueStep(site.convert_function, &.{ map, key_root, found });
                }
            } else {
                const item = try self.decodeValue(items.item, depth + 1);
                try self.vm.setMapEntry(&self.vm.heap.temporary_roots.items[map].value, &self.vm.heap.temporary_roots.items[key_root].value, &self.vm.heap.temporary_roots.items[item].value);
            }
            self.path.shrinkRetainingCapacity(path_base);
        }
        _ = try self.nextToken();
        return map;
    }

    fn decodeSite(self: *Decoder, site: serialization_plan.ProxySite, depth: usize) !?usize {
        const is_null = site.is_nullable_target and try self.peekToken() == .null;
        if (is_null) {
            _ = try self.nextToken();
            return null;
        }
        return try self.decodeValue(site.plan, depth);
    }

    fn queueStep(self: *Decoder, function: u16, captures: []const usize) !void {
        const size = v.Closure.calc_size(captures.len);
        try self.vm.maybeCollectGarbage(size);
        const closure = try v.Closure.init(self.vm.getCurrentHeapAllocator(), &self.vm.program.functions[function], @intCast(captures.len));
        self.vm.increaseHeapAllocationCount(size);
        for (captures, 0..) |root, i| {
            closure.captures()[i] = self.vm.heap.protectedValue(root);
        }
        const rooted = try self.vm.heap.protectValue(.{ .heap_obj = &closure.obj }, .function);
        try self.appendArrayItem(self.steps, rooted);
    }

    fn readUnion(self: *Decoder, shape: serialization_plan.TaggedUnion, depth: usize) !usize {
        try self.enterContainer(.object_begin, depth);
        if (try self.peekToken() == .object_end) {
            return error.InvalidUnion;
        }
        var keys = std.StringHashMap(void).init(self.allocator);
        const key = try self.readObjectKey(&keys);
        const path_base = self.path.items.len;
        try self.appendFieldPath(key, false);
        const variant = for (shape.variants) |candidate| {
            if (candidate.isNamed(key)) {
                break candidate;
            }
        } else return error.UnknownVariant;
        const item = try self.decodeValue(variant.plan, depth + 1);
        if (try self.peekToken() != .object_end) {
            _ = try self.readObjectKey(&keys);
            return error.InvalidUnion;
        }
        _ = try self.nextToken();
        const size = v.Union.calc_size();
        try self.vm.maybeCollectGarbage(size);
        const object = try v.Union.init(self.vm.getCurrentHeapAllocator(), shape.descriptor);
        self.vm.increaseHeapAllocationCount(size);
        object.set(variant.tag, self.vm.heap.protectedValue(item));
        const result = try self.vm.heap.protectValue(.{ .heap_obj = &object.obj }, .@"union");
        self.path.shrinkRetainingCapacity(path_base);
        return result;
    }

    fn readObjectKey(self: *Decoder, keys: *std.StringHashMap(void)) ![]const u8 {
        const name = try self.readStringText();
        const entry = try keys.getOrPut(name);
        if (entry.found_existing) {
            try self.appendFieldPath(name, true);
            return error.DuplicateKey;
        }
        return name;
    }

    fn skipValue(self: *Decoder, depth: usize) anyerror!void {
        switch (try self.peekToken()) {
            .object_begin => {
                try self.enterContainer(.object_begin, depth);
                var keys = std.StringHashMap(void).init(self.allocator);
                while (try self.peekToken() != .object_end) {
                    const key_text = try self.readObjectKey(&keys);
                    const path_base = self.path.items.len;
                    try self.appendFieldPath(key_text, false);
                    try self.skipValue(depth + 1);
                    self.path.shrinkRetainingCapacity(path_base);
                }
                _ = try self.nextToken();
            },
            .array_begin => {
                try self.enterContainer(.array_begin, depth);
                var i: usize = 0;
                while (try self.peekToken() != .array_end) : (i += 1) {
                    const path_base = self.path.items.len;
                    try self.appendIndexPath(i);
                    try self.skipValue(depth + 1);
                    self.path.shrinkRetainingCapacity(path_base);
                }
                _ = try self.nextToken();
            },
            .string, .number, .true, .false, .null => {
                _ = try self.nextToken();
            },
            else => return error.InvalidJson,
        }
    }

    fn peekToken(self: *Decoder) !std.json.TokenType {
        return self.scanner.peekNextTokenType() catch |failure| {
            self.offset = self.scanner.cursor;
            return failure;
        };
    }

    fn nextToken(self: *Decoder) !std.json.Token {
        _ = try self.peekToken();
        self.offset = self.scanner.cursor;
        return self.scanner.nextAllocMax(self.allocator, .alloc_always, self.source.len) catch |failure| {
            self.offset = self.scanner.cursor;
            return failure;
        };
    }

    fn appendIndexPath(self: *Decoder, index: usize) !void {
        if (!self.hasDiagnostic()) return;
        try self.path.appendSlice(self.allocator, try std.fmt.allocPrint(self.allocator, "[{d}]", .{index}));
    }

    fn hasDiagnostic(self: *Decoder) bool {
        return @as(u64, @bitCast(self.vm.heap.protectedValue(self.diagnostic))) != 0;
    }

    fn appendFieldPath(self: *Decoder, name: []const u8, is_map_key: bool) !void {
        if (!self.hasDiagnostic()) return;
        var is_identifier = name.len > 0;
        for (name, 0..) |c, i| {
            const is_valid = std.ascii.isAlphabetic(c) or c == '_' or (i > 0 and std.ascii.isDigit(c));
            if (!is_valid) {
                is_identifier = false;
                break;
            }
        }
        const can_use_identifier = is_identifier and !is_map_key;
        if (can_use_identifier) {
            if (self.path.items.len > 0) {
                try self.path.append(self.allocator, '.');
            }
            try self.path.appendSlice(self.allocator, name);
            return;
        }
        try self.path.append(self.allocator, '[');
        try self.path.appendSlice(self.allocator, try std.json.Stringify.valueAlloc(self.allocator, name, .{}));
        try self.path.append(self.allocator, ']');
    }

    fn allocRootedStruct(self: *Decoder, descriptor: u16) !usize {
        const count = self.vm.program.struct_descriptors[descriptor].field_count;
        const size = v.Struct.calc_size(count);
        try self.vm.maybeCollectGarbage(size);
        const object = try v.Struct.init(self.vm.getCurrentHeapAllocator(), count, descriptor);
        self.vm.increaseHeapAllocationCount(size);
        return self.vm.heap.protectValue(.{ .heap_obj = &object.obj }, .@"struct");
    }

    fn rootedStruct(self: *Decoder, root: usize) *v.Struct {
        return v.HeapValue.getParentPtr(v.Struct, self.vm.heap.protectedValue(root).heap_obj);
    }

    fn allocRootedArray(self: *Decoder, kind: v.ValueType) !usize {
        const size = v.Array.calc_size(8);
        try self.vm.maybeCollectGarbage(size);
        const array = try v.Array.init(self.vm.getCurrentHeapAllocator(), kind, 8);
        self.vm.increaseHeapAllocationCount(size);
        return self.vm.heap.protectValue(.{ .heap_obj = &array.obj }, .array);
    }

    fn appendArrayItem(self: *Decoder, array_root: usize, item_root: usize) !void {
        var array = v.HeapValue.getParentPtr(v.Array, self.vm.heap.protectedValue(array_root).heap_obj);
        if (array.len == array.capacity) {
            array = try self.vm.growArray(&self.vm.heap.temporary_roots.items[array_root].value);
        }
        try array.append(self.vm.heap.protectedValue(item_root));
    }

    fn resetDiagnostic(self: *Decoder) !void {
        if (@as(u64, @bitCast(self.vm.heap.protectedValue(self.diagnostic))) == 0) return;
        const empty = try self.vm.allocBytesValue("", .string);
        const diagnostic = self.rootedStruct(self.diagnostic);
        diagnostic.set(0, empty);
        for (1..4) |i| {
            diagnostic.set(i, v.NULL_VALUE);
        }
    }

    fn writeDiagnostic(self: *Decoder) !void {
        if (@as(u64, @bitCast(self.vm.heap.protectedValue(self.diagnostic))) == 0) return;
        const path = try self.vm.allocBytesValue(self.path.items, .string);
        self.rootedStruct(self.diagnostic).set(0, path);
        const offset = @min(self.offset, self.source.len);
        var line: i64 = 1;
        var column: i64 = 1;
        for (self.source[0..offset]) |c| {
            if (c == '\n') {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        const positions = [_]i64{ @intCast(offset), line, column };
        for (positions, 1..) |position, i| {
            const boxed = try self.vm.allocNullBox(.{ .int64 = position }, .int64);
            self.rootedStruct(self.diagnostic).set(i, boxed);
        }
    }
};
