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

var scratch: std.heap.ArenaAllocator = .init(std.heap.c_allocator);
var encoded: std.ArrayList(u8) = .empty;

const kept_output_capacity = 1 << 20;

fn releaseScratch() void {
    _ = scratch.reset(.{ .retain_with_limit = 1 << 20 });
}

pub fn encode(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    defer releaseScratch();
    const allocator = scratch.allocator();
    const max_depth = encodeDepth(args[1]) orelse return native.fail(JsonError.InvalidOptions);
    encoded.clearRetainingCapacity();
    defer releaseEncodedOutput();
    var encoder = Encoder{ .vm = vm, .allocator = allocator, .max_depth = max_depth };
    if (args.len > 3) {
        try encoder.levels.append(allocator, .{ .list = arrayOf(args[3]) });
    }
    encoder.visit(.write, @intCast(args[2].int64), args[0], 0) catch |failure| {
        return encodeFailure(failure);
    };
    return native.ok(try vm.allocBytesValue(encoded.items, .string));
}

fn releaseEncodedOutput() void {
    if (encoded.capacity <= kept_output_capacity) return;
    encoded.clearAndFree(std.heap.c_allocator);
}

pub fn collectSites(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    defer releaseScratch();
    const root_base = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_base);
    const max_depth = encodeDepth(args[1]) orelse return native.fail(JsonError.InvalidOptions);
    var encoder = Encoder{ .vm = vm, .allocator = scratch.allocator(), .max_depth = max_depth };
    const roots = try vm.heap.protectValue(args[3], .array);
    const queue = try vm.heap.protectValue(args[4], .array);
    encoder.visit(.collect, @intCast(args[2].int64), args[0], 0) catch |failure| {
        return encodeFailure(failure);
    };
    try encoder.createSiteClosures(roots, queue);
    return native.ok(.{ .null = {} });
}

pub fn collectNested(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    defer releaseScratch();
    const root_base = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_base);
    const max_depth = encodeDepth(args[3]) orelse return native.fail(JsonError.InvalidOptions);
    var encoder = Encoder{ .vm = vm, .allocator = scratch.allocator(), .max_depth = max_depth };
    const queue = try vm.heap.protectValue(args[0], .array);
    const stop: usize = @intCast(args[2].int64);
    var i: usize = @intCast(args[1].int64);
    while (i < stop) : (i += 1) {
        const closure = closureOf(arrayOf(vm.heap.protectedValue(queue)).getDataPtr()[i]);
        const out = arrayOf(closure.captures()[1]);
        if (out.len == 0) continue;
        const children = try vm.heap.protectValue(closure.captures()[2], .array);
        encoder.found.clearRetainingCapacity();
        encoder.visit(.collect, @intCast(closure.captures()[3].int64), out.getDataPtr()[0], 0) catch |failure| {
            return encodeFailure(failure);
        };
        try encoder.createSiteClosures(children, queue);
    }
    return native.ok(.{ .null = {} });
}

fn encodeDepth(options_value: v.Value) ?usize {
    const options = v.HeapValue.getParentPtr(v.Struct, options_value.heap_obj);
    const max_depth = options.get(0).int64;
    if (!isValidDepth(max_depth)) {
        return null;
    }
    return @intCast(max_depth);
}

fn encodeFailure(failure: anyerror) anyerror!native.NativeResult {
    const is_allocation_failure = failure == error.OutOfMemory or failure == error.WriteFailed;
    if (is_allocation_failure) {
        return error.OutOfMemory;
    }
    return native.fail(std.meta.stringToEnum(JsonError, @errorName(failure)) orelse .InvalidJson);
}

fn isValidDepth(depth: i64) bool {
    return depth > 0 and depth <= 512;
}

const Mode = enum { collect, write };

const Level = struct {
    list: *v.Array,
    next: usize = 0,
};

const FoundSite = struct {
    value: usize,
    site: serialization_plan.ProxySite,
};

const Encoder = struct {
    vm: *VM,
    allocator: std.mem.Allocator,
    max_depth: usize,
    ancestors: std.ArrayList(*v.HeapValue) = .empty,
    levels: std.ArrayList(Level) = .empty,
    found: std.ArrayList(FoundSite) = .empty,

    fn visit(self: *Encoder, comptime mode: Mode, plan_index: u32, value: v.Value, depth: usize) anyerror!void {
        const plan = self.vm.program.serialization_plans[plan_index];
        if (plan.shape == .nullable) {
            if (@as(u64, @bitCast(value)) == 0) {
                if (mode == .write) {
                    try writeAll("null");
                }
                return;
            }
            const inner = self.vm.program.serialization_plans[plan.shape.nullable];
            const unwrapped = if (inner.kind.isHeapType()) value else v.HeapValue.getParentPtr(v.NullBox, value.heap_obj).value;
            return self.visit(mode, plan.shape.nullable, unwrapped, depth);
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
            .int, .byte, .float, .boolean, .string, .enumeration => {
                if (mode == .collect) return;
                try writeScalar(plan.shape, value);
            },
            .structure => |structure| {
                const object = v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
                if (mode == .write) {
                    try writeByte('{');
                }
                var is_first = true;
                for (structure.fields) |field| {
                    const item = object.get(field.slot);
                    const is_omitted = field.omits_null and @as(u64, @bitCast(item)) == 0;
                    if (is_omitted) continue;
                    if (mode == .write) {
                        try writeKey(field.name, is_first);
                    }
                    is_first = false;
                    if (field.proxy) |site| {
                        try self.visitSite(mode, site, item, self.planKind(field.plan), depth + 1);
                        continue;
                    }
                    try self.visit(mode, field.plan, item, depth + 1);
                }
                if (mode == .write) {
                    try writeByte('}');
                }
            },
            .array => |items| {
                const array = arrayOf(value);
                if (mode == .write) {
                    try writeByte('[');
                }
                for (array.getDataPtr()[0..array.len], 0..) |item, index| {
                    if (mode == .write and index > 0) {
                        try writeByte(',');
                    }
                    if (items.proxy) |site| {
                        try self.visitSite(mode, site, item, self.planKind(items.item), depth + 1);
                        continue;
                    }
                    try self.visit(mode, items.item, item, depth + 1);
                }
                if (mode == .write) {
                    try writeByte(']');
                }
            },
            .map => |items| {
                const map = v.HeapValue.getParentPtr(v.Map, value.heap_obj);
                if (mode == .write) {
                    try writeByte('{');
                }
                if (map.getStorage()) |storage| {
                    var is_first = true;
                    for (storage.entries()[0..storage.len]) |entry| {
                        if (entry.isRemoved()) continue;
                        if (mode == .write) {
                            const key = bytes(entry.key);
                            if (!std.unicode.utf8ValidateSlice(key)) {
                                return error.InvalidJson;
                            }
                            try writeKey(key, is_first);
                        }
                        is_first = false;
                        if (items.proxy) |site| {
                            try self.visitSite(mode, site, entry.value, self.planKind(items.item), depth + 1);
                            continue;
                        }
                        try self.visit(mode, items.item, entry.value, depth + 1);
                    }
                }
                if (mode == .write) {
                    try writeByte('}');
                }
            },
            .tagged_union => |tagged| {
                const object = v.HeapValue.getParentPtr(v.Union, value.heap_obj);
                const variant = for (tagged.variants) |candidate| {
                    if (candidate.tag == object.active_tag) {
                        break candidate;
                    }
                } else return error.InvalidUnion;
                if (mode == .write) {
                    try writeByte('{');
                    try writeKey(variant.name, true);
                }
                if (variant.proxy) |site| {
                    try self.visitSite(mode, site, object.get(variant.tag), self.planKind(variant.plan), depth + 1);
                } else {
                    try self.visit(mode, variant.plan, object.get(variant.tag), depth + 1);
                }
                if (mode == .write) {
                    try writeByte('}');
                }
            },
            .proxy => |site| try self.visitSite(mode, site, value, plan.kind, depth),
            .nullable => unreachable,
        }
    }

    fn planKind(self: *Encoder, plan_index: u32) v.ValueType {
        return self.vm.program.serialization_plans[plan_index].kind;
    }

    fn visitSite(self: *Encoder, comptime mode: Mode, site: serialization_plan.ProxySite, value: v.Value, kind: v.ValueType, depth: usize) !void {
        const is_null = site.is_nullable_target and @as(u64, @bitCast(value)) == 0;
        if (is_null) {
            if (mode == .write) {
                try writeAll("null");
            }
            return;
        }
        if (mode == .collect) {
            const root = try self.vm.heap.protectValue(value, kind);
            try self.found.append(self.allocator, .{ .value = root, .site = site });
            return;
        }
        if (self.levels.items.len == 0) {
            return error.InvalidJson;
        }
        const level = &self.levels.items[self.levels.items.len - 1];
        if (level.next >= level.list.len) {
            return error.InvalidJson;
        }
        const closure = closureOf(level.list.getDataPtr()[level.next]);
        level.next += 1;
        const out = arrayOf(closure.captures()[1]);
        if (out.len == 0) {
            return error.InvalidJson;
        }
        try self.levels.append(self.allocator, .{ .list = arrayOf(closure.captures()[2]) });
        defer _ = self.levels.pop();
        try self.visit(.write, site.plan, out.getDataPtr()[0], depth);
    }

    fn createSiteClosures(self: *Encoder, level: usize, queue: usize) !void {
        for (self.found.items) |entry| {
            const out = try allocRootedArray(self.vm, self.planKind(entry.site.plan), 1);
            const children = try allocRootedArray(self.vm, .function, 2);
            const plan = try self.vm.heap.protectValue(.{ .int64 = entry.site.plan }, .int64);
            const closure = try allocRootedClosure(self.vm, entry.site.encode_function, &.{ entry.value, out, children, plan });
            try appendRootedItem(self.vm, level, closure);
            try appendRootedItem(self.vm, queue, closure);
        }
    }
};

fn writeScalar(shape: serialization_plan.Shape, value: v.Value) !void {
    switch (shape) {
        .int, .byte => try writeInt(value.int64),
        .float => {
            if (!std.math.isFinite(value.float64)) {
                return error.NonFiniteNumber;
            }
            try writeFloat(value.float64);
        },
        .boolean => try writeAll(if (value.bool) "true" else "false"),
        .string => {
            const text = bytes(value);
            if (!std.unicode.utf8ValidateSlice(text)) {
                return error.InvalidJson;
            }
            try writeString(text);
        },
        .enumeration => |variants| {
            for (variants) |variant| {
                if (variant.value != value.int64) continue;
                try writeString(variant.name);
                return;
            }
            return error.UnknownVariant;
        },
        else => unreachable,
    }
}

fn writeKey(name: []const u8, is_first: bool) !void {
    try encoded.ensureUnusedCapacity(std.heap.c_allocator, longestEscapedSize(name.len) + 2);
    if (!is_first) {
        encoded.appendAssumeCapacity(',');
    }
    appendStringAssumeCapacity(name);
    encoded.appendAssumeCapacity(':');
}

inline fn writeByte(byte: u8) !void {
    if (encoded.items.len < encoded.capacity) {
        encoded.appendAssumeCapacity(byte);
        return;
    }
    try encoded.append(std.heap.c_allocator, byte);
}

fn writeAll(text: []const u8) !void {
    try encoded.appendSlice(std.heap.c_allocator, text);
}

fn writeInt(number: i64) !void {
    var digits: [20]u8 = undefined;
    var position: usize = digits.len;
    var magnitude: u64 = @abs(number);
    while (true) {
        position -= 1;
        digits[position] = @intCast('0' + magnitude % 10);
        magnitude /= 10;
        if (magnitude == 0) break;
    }
    try encoded.ensureUnusedCapacity(std.heap.c_allocator, digits.len - position + 1);
    if (number < 0) {
        encoded.appendAssumeCapacity('-');
    }
    for (digits[position..]) |digit| {
        encoded.appendAssumeCapacity(digit);
    }
}

fn writeFloat(number: f64) !void {
    var text: [std.fmt.float.bufferSize(.decimal, f64) + 1]u8 = undefined;
    try writeAll(std.fmt.bufPrint(&text, "{}", .{number}) catch unreachable);
}

fn writeString(text: []const u8) !void {
    try encoded.ensureUnusedCapacity(std.heap.c_allocator, longestEscapedSize(text.len));
    appendStringAssumeCapacity(text);
}

fn longestEscapedSize(length: usize) usize {
    return length * 6 + 2;
}

fn appendStringAssumeCapacity(text: []const u8) void {
    const out = encoded.unusedCapacitySlice();
    out[0] = '"';
    var written: usize = 1;
    for (text) |char| {
        if (!needsEscape(char)) {
            out[written] = char;
            written += 1;
            continue;
        }
        written += writeEscapeInto(out[written..], char);
    }
    out[written] = '"';
    encoded.items.len += written + 1;
}

fn needsEscape(char: u8) bool {
    return char < 0x20 or char == '"' or char == '\\';
}

fn writeEscapeInto(out: []u8, char: u8) usize {
    const short: ?u8 = switch (char) {
        '\\' => '\\',
        '"' => '"',
        0x08 => 'b',
        0x0C => 'f',
        '\n' => 'n',
        '\r' => 'r',
        '\t' => 't',
        else => null,
    };
    out[0] = '\\';
    if (short) |letter| {
        out[1] = letter;
        return 2;
    }
    const hex = "0123456789abcdef";
    @memcpy(out[1..6], &[_]u8{ 'u', '0', '0', hex[char >> 4], hex[char & 0xF] });
    return 6;
}

fn bytes(value: v.Value) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, value.heap_obj).slice();
}

fn arrayOf(value: v.Value) *v.Array {
    return v.HeapValue.getParentPtr(v.Array, value.heap_obj).newestCopy();
}

fn closureOf(value: v.Value) *v.Closure {
    return v.HeapValue.getParentPtr(v.Closure, value.heap_obj);
}

fn allocRootedArray(vm: *VM, kind: v.ValueType, capacity: usize) !usize {
    const size = v.Array.calc_size(capacity);
    try vm.maybeCollectGarbage(size);
    const array = try v.Array.init(vm.getCurrentHeapAllocator(), kind, capacity);
    vm.increaseHeapAllocationCount(size);
    return vm.heap.protectValue(.{ .heap_obj = &array.obj }, .array);
}

fn appendRootedItem(vm: *VM, array_root: usize, item_root: usize) !void {
    var array = arrayOf(vm.heap.protectedValue(array_root));
    if (array.len == array.capacity) {
        array = try vm.growArray(&vm.heap.temporary_roots.items[array_root].value);
    }
    try array.append(vm.heap.protectedValue(item_root));
}

fn allocRootedClosure(vm: *VM, function: u16, captures: []const usize) !usize {
    const size = v.Closure.calc_size(captures.len);
    try vm.maybeCollectGarbage(size);
    const closure = try v.Closure.init(vm.getCurrentHeapAllocator(), &vm.program.functions[function], captures.len);
    vm.increaseHeapAllocationCount(size);
    for (captures, 0..) |root, i| {
        closure.captures()[i] = vm.heap.protectedValue(root);
    }
    return vm.heap.protectValue(.{ .heap_obj = &closure.obj }, .function);
}

pub fn decode(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return decodeDocument(vm, args, false);
}

pub fn decodeProxied(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return decodeDocument(vm, args, true);
}

pub fn describeStep(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    if (@as(u64, @bitCast(args[2])) == 0) {
        return native.ok(.{ .null = {} });
    }
    const root_base = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_base);
    const closure = closureOf(args[0]);
    const path = closure.captures()[closure.capture_count - 2];
    const offset: usize = @intCast(closure.captures()[closure.capture_count - 1].int64);
    const position = Position.at(bytes(args[1]), offset);
    const diagnostic = try vm.heap.protectValue(args[2], .nullable);
    const path_value = if (@as(u64, @bitCast(path)) == 0) try vm.allocBytesValue("", .string) else path;
    try writeDiagnosticFields(vm, diagnostic, path_value, position);
    return native.ok(.{ .null = {} });
}

fn decodeDocument(vm: *VM, args: []v.Value, has_holder: bool) anyerror!native.NativeResult {
    defer releaseScratch();
    const allocator = scratch.allocator();
    const root_base = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_base);
    const diagnostic = try vm.heap.protectValue(args[1], .nullable);
    const steps = try vm.heap.protectValue(args[4], .array);
    const holder = if (has_holder) try vm.heap.protectValue(args[5], .array) else null;
    const source = try allocator.dupe(u8, bytes(args[0]));
    const options = v.HeapValue.getParentPtr(v.Struct, args[2].heap_obj);
    const max_depth = options.get(1).int64;
    var decoder = Decoder{
        .vm = vm,
        .allocator = allocator,
        .source = source,
        .lexer = .{ .source = source, .allocator = allocator },
        .diagnostic = diagnostic,
        .max_depth = if (isValidDepth(max_depth)) @intCast(max_depth) else 0,
        .should_reject_unknown_fields = options.get(0).bool,
        .steps = steps,
    };
    try decoder.resetDiagnostic();
    const result = decoder.decodeRoot(@intCast(args[3].int64), holder) catch |failure| {
        if (failure == error.OutOfMemory) {
            return failure;
        }
        try decoder.writeDiagnostic();
        return native.fail(std.meta.stringToEnum(JsonError, @errorName(failure)) orelse .InvalidJson);
    };
    return native.ok(result);
}

fn findField(fields: []const serialization_plan.Field, key: []const u8, guess: usize) ?usize {
    if (guess < fields.len and fields[guess].isNamed(key)) {
        return guess;
    }
    for (fields, 0..) |field, i| {
        if (field.isNamed(key)) {
            return i;
        }
    }
    return null;
}

const SeenFields = struct {
    small: u64 = 0,
    large: ?[]bool = null,

    fn init(allocator: std.mem.Allocator, count: usize) !SeenFields {
        if (count <= 64) {
            return .{};
        }
        const large = try allocator.alloc(bool, count);
        @memset(large, false);
        return .{ .large = large };
    }

    fn isSet(self: SeenFields, index: usize) bool {
        if (self.large) |large| {
            return large[index];
        }
        return self.small & (@as(u64, 1) << @intCast(index)) != 0;
    }

    fn set(self: *SeenFields, index: usize) void {
        if (self.large) |large| {
            large[index] = true;
            return;
        }
        self.small |= @as(u64, 1) << @intCast(index);
    }
};

const PendingItem = struct {
    index: usize,
    site_value: SiteValue,
};

const SiteValue = struct {
    proxy_value: usize,
    path: usize,
    offset: usize,
};

const Decoder = struct {
    vm: *VM,
    allocator: std.mem.Allocator,
    source: []const u8,
    lexer: Lexer,
    diagnostic: usize,
    max_depth: usize,
    should_reject_unknown_fields: bool,
    path: std.ArrayList(u8) = .empty,
    steps: usize,

    fn decodeRoot(self: *Decoder, plan_index: u32, holder: ?usize) !v.Value {
        if (self.max_depth == 0) {
            return error.InvalidOptions;
        }
        const value = if (holder) |found| try self.decodeIntoHolder(plan_index, found) else try self.decodeValue(plan_index, 0);
        if (self.lexer.peekKind() != .end) {
            return error.InvalidJson;
        }
        return self.vm.heap.protectedValue(value);
    }

    fn decodeIntoHolder(self: *Decoder, plan_index: u32, holder: usize) !usize {
        const plan = self.vm.program.serialization_plans[plan_index];
        const site = plan.root_site.?;
        const placeholder = try self.vm.heap.protectValue(v.NULL_VALUE, plan.kind);
        try appendRootedItem(self.vm, holder, placeholder);
        const site_value = try self.decodeSite(site, 0) orelse return placeholder;
        const index = try self.vm.heap.protectValue(.{ .int64 = 0 }, .int64);
        try self.queueConversion(site.decode_function, &.{ holder, index }, site_value);
        return placeholder;
    }

    fn decodeValue(self: *Decoder, plan_index: u32, depth: usize) anyerror!usize {
        const plan = self.vm.program.serialization_plans[plan_index];
        if (plan.shape == .nullable) {
            if (self.lexer.peekKind() == .literal_null) {
                try self.lexer.readLiteral("null");
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
                const found = try self.readNumber();
                if (!found.is_integer) {
                    return error.TypeMismatch;
                }
                const number = std.fmt.parseInt(i64, found.text, 10) catch return error.NumberOutOfRange;
                const is_byte_out_of_range = plan.shape == .byte and (number < 0 or number > 255);
                if (is_byte_out_of_range) {
                    return error.NumberOutOfRange;
                }
                return self.vm.heap.protectValue(.{ .int64 = number }, .int64);
            },
            .float => {
                const found = try self.readNumber();
                const number = std.fmt.parseFloat(f64, found.text) catch return error.NumberOutOfRange;
                if (!std.math.isFinite(number)) {
                    return error.NumberOutOfRange;
                }
                return self.vm.heap.protectValue(.{ .float64 = number }, .float64);
            },
            .boolean => {
                const kind = self.lexer.peekKind();
                const is_true = switch (kind) {
                    .literal_true => true,
                    .literal_false => false,
                    else => return self.rejectToken(kind),
                };
                try self.lexer.readLiteral(if (is_true) "true" else "false");
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
                try self.enterContainer(.array, depth);
                const item_kind = self.vm.program.serialization_plans[items.item].kind;
                const array = try allocRootedArray(self.vm, item_kind, 8);
                var pending: std.ArrayList(PendingItem) = .empty;
                var is_first = true;
                var i: usize = 0;
                while (try self.lexer.hasItem(&is_first)) : (i += 1) {
                    const path_base = self.path.items.len;
                    try self.appendIndexPath(i);
                    if (items.proxy) |site| {
                        const placeholder = try self.vm.heap.protectValue(v.NULL_VALUE, item_kind);
                        if (try self.decodeSite(site, depth + 1)) |site_value| {
                            try pending.append(self.allocator, .{ .index = i, .site_value = site_value });
                        }
                        try appendRootedItem(self.vm, array, placeholder);
                    } else {
                        const item = try self.decodeValue(items.item, depth + 1);
                        try appendRootedItem(self.vm, array, item);
                    }
                    self.path.shrinkRetainingCapacity(path_base);
                }
                if (items.proxy) |site| {
                    for (pending.items) |entry| {
                        const index = try self.vm.heap.protectValue(.{ .int64 = @intCast(entry.index) }, .int64);
                        try self.queueConversion(site.decode_function, &.{ array, index }, entry.site_value);
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

    fn readNumber(self: *Decoder) !Number {
        const kind = self.lexer.peekKind();
        if (kind != .number) {
            return self.rejectToken(kind);
        }
        return self.lexer.readNumber();
    }

    fn readStringText(self: *Decoder) ![]const u8 {
        const kind = self.lexer.peekKind();
        if (kind != .string) {
            return self.rejectToken(kind);
        }
        return self.lexer.readString();
    }

    fn rejectToken(self: *Decoder, kind: ValueKind) anyerror {
        switch (kind) {
            .string => _ = try self.lexer.readString(),
            .number => _ = try self.lexer.readNumber(),
            .literal_true => try self.lexer.readLiteral("true"),
            .literal_false => try self.lexer.readLiteral("false"),
            .literal_null => try self.lexer.readLiteral("null"),
            .object, .array => {},
            .end, .invalid => return error.InvalidJson,
        }
        return error.TypeMismatch;
    }

    fn enterContainer(self: *Decoder, expected: ValueKind, depth: usize) !void {
        const kind = self.lexer.peekKind();
        if (kind != expected) {
            return self.rejectToken(kind);
        }
        self.lexer.pos += 1;
        if (depth >= self.max_depth) {
            return error.DepthExceeded;
        }
    }

    fn readStruct(self: *Decoder, shape: serialization_plan.Structure, depth: usize) !usize {
        try self.enterContainer(.object, depth);
        const object = try self.allocRootedStruct(shape.descriptor);
        var seen_fields = try SeenFields.init(self.allocator, shape.fields.len);
        var unknown_keys: ?std.StringHashMap(void) = null;
        var guess: usize = 0;
        var is_first = true;
        while (try self.lexer.nextKey(&is_first)) |key| {
            const path_base = self.path.items.len;
            try self.appendFieldPath(key, false);
            const field_index = findField(shape.fields, key, guess);
            if (field_index) |i| {
                if (seen_fields.isSet(i)) {
                    return error.DuplicateKey;
                }
                seen_fields.set(i);
                guess = i + 1;
                if (shape.fields[i].proxy) |site| {
                    if (try self.decodeSite(site, depth + 1)) |site_value| {
                        try self.queueConversion(site.decode_function, &.{object}, site_value);
                    }
                } else {
                    const item = try self.decodeValue(shape.fields[i].plan, depth + 1);
                    self.rootedStruct(object).set(shape.fields[i].slot, self.vm.heap.protectedValue(item));
                }
            } else {
                if (self.should_reject_unknown_fields) {
                    return error.UnknownField;
                }
                if (unknown_keys == null) {
                    unknown_keys = std.StringHashMap(void).init(self.allocator);
                }
                const entry = try unknown_keys.?.getOrPut(key);
                if (entry.found_existing) {
                    return error.DuplicateKey;
                }
                try self.skipValue(depth + 1);
            }
            self.path.shrinkRetainingCapacity(path_base);
        }
        for (shape.skipped) |skip| {
            try self.applyDefault(object, skip.slot, skip.default);
        }
        for (shape.fields, 0..) |field, i| {
            if (seen_fields.isSet(i)) continue;
            if (field.default) |default| {
                try self.applyDefault(object, field.slot, default);
                continue;
            }
            try self.appendFieldPath(field.name, false);
            return error.MissingField;
        }
        return object;
    }

    fn readMap(self: *Decoder, items: serialization_plan.Items, depth: usize) !usize {
        try self.enterContainer(.object, depth);
        const value_kind = self.vm.program.serialization_plans[items.item].kind;
        const value = try self.vm.newMap(.string, value_kind);
        const map = try self.vm.heap.protectValue(value, .map);
        var is_first = true;
        while (try self.lexer.nextKey(&is_first)) |key_text| {
            const path_base = self.path.items.len;
            try self.appendFieldPath(key_text, true);
            const key_value = try self.vm.allocBytesValue(key_text, .string);
            const map_object = v.HeapValue.getParentPtr(v.Map, self.vm.heap.protectedValue(map).heap_obj);
            if (map_object.find(key_value, map_object.hashKey(key_value)) != null) {
                return error.DuplicateKey;
            }
            const key_root = try self.vm.heap.protectValue(key_value, .string);
            if (items.proxy) |site| {
                const placeholder = try self.vm.heap.protectValue(v.NULL_VALUE, value_kind);
                const site_value = try self.decodeSite(site, depth + 1);
                try self.vm.setMapEntry(&self.vm.heap.temporary_roots.items[map].value, &self.vm.heap.temporary_roots.items[key_root].value, &self.vm.heap.temporary_roots.items[placeholder].value);
                if (site_value) |found| {
                    try self.queueConversion(site.decode_function, &.{ map, key_root }, found);
                }
            } else {
                const item = try self.decodeValue(items.item, depth + 1);
                try self.vm.setMapEntry(&self.vm.heap.temporary_roots.items[map].value, &self.vm.heap.temporary_roots.items[key_root].value, &self.vm.heap.temporary_roots.items[item].value);
            }
            self.path.shrinkRetainingCapacity(path_base);
        }
        return map;
    }

    fn applyDefault(self: *Decoder, object: usize, slot: u8, default: serialization_plan.Default) !void {
        switch (default) {
            .function => |function| try self.queueStep(function, &.{object}),
            .constant => |constant| {
                const value = try self.constantRoot(constant.value);
                const stored = if (constant.box_kind) |kind| try self.vm.allocNullBox(self.vm.heap.protectedValue(value), kind) else self.vm.heap.protectedValue(value);
                self.rootedStruct(object).set(slot, stored);
            },
        }
    }

    fn constantRoot(self: *Decoder, constant: serialization_plan.ConstantValue) !usize {
        return switch (constant) {
            .int => |number| self.vm.heap.protectValue(.{ .int64 = number }, .int64),
            .float => |number| self.vm.heap.protectValue(.{ .float64 = number }, .float64),
            .boolean => |flag| self.vm.heap.protectValue(.{ .bool = flag }, .bool),
            .null => self.vm.heap.protectValue(v.NULL_VALUE, .nullable),
            .string => |id| self.vm.heap.protectValue(self.vm.program.string_pool.values[id], .string),
            .array => |array| blk: {
                const root = try allocRootedArray(self.vm, array.kind, @max(array.items.len, 1));
                for (array.items) |item| {
                    try appendRootedItem(self.vm, root, try self.constantRoot(item));
                }
                break :blk root;
            },
        };
    }

    fn decodeSite(self: *Decoder, site: serialization_plan.ProxySite, depth: usize) !?SiteValue {
        const next = self.lexer.peekKind();
        const is_null = site.is_nullable_target and next == .literal_null;
        if (is_null) {
            try self.lexer.readLiteral("null");
            return null;
        }
        const offset = try self.vm.heap.protectValue(.{ .int64 = @intCast(self.lexer.token_start) }, .int64);
        const path_value = if (self.hasDiagnostic()) try self.vm.allocBytesValue(self.path.items, .string) else v.NULL_VALUE;
        const path = try self.vm.heap.protectValue(path_value, .nullable);
        const proxy_value = try self.decodeValue(site.plan, depth);
        return .{ .proxy_value = proxy_value, .path = path, .offset = offset };
    }

    fn queueStep(self: *Decoder, function: u16, captures: []const usize) !void {
        const closure = try allocRootedClosure(self.vm, function, captures);
        try appendRootedItem(self.vm, self.steps, closure);
    }

    fn queueConversion(self: *Decoder, function: u16, targets: []const usize, site_value: SiteValue) !void {
        var captures: [5]usize = undefined;
        @memcpy(captures[0..targets.len], targets);
        captures[targets.len] = site_value.proxy_value;
        captures[targets.len + 1] = site_value.path;
        captures[targets.len + 2] = site_value.offset;
        try self.queueStep(function, captures[0 .. targets.len + 3]);
    }

    fn readUnion(self: *Decoder, shape: serialization_plan.TaggedUnion, depth: usize) !usize {
        try self.enterContainer(.object, depth);
        var is_first = true;
        const key = try self.lexer.nextKey(&is_first) orelse return error.InvalidUnion;
        const path_base = self.path.items.len;
        try self.appendFieldPath(key, false);
        const variant = for (shape.variants) |candidate| {
            if (candidate.isNamed(key)) {
                break candidate;
            }
        } else return error.UnknownVariant;
        var site_value: ?SiteValue = null;
        const item = if (variant.proxy) |site| blk: {
            site_value = try self.decodeSite(site, depth + 1);
            break :blk try self.vm.heap.protectValue(v.NULL_VALUE, self.vm.program.serialization_plans[variant.plan].kind);
        } else try self.decodeValue(variant.plan, depth + 1);
        if (try self.lexer.nextKey(&is_first)) |second| {
            if (std.mem.eql(u8, second, key)) {
                try self.appendFieldPath(second, true);
                return error.DuplicateKey;
            }
            return error.InvalidUnion;
        }
        const size = v.Union.calc_size();
        try self.vm.maybeCollectGarbage(size);
        const object = try v.Union.init(self.vm.getCurrentHeapAllocator(), shape.descriptor);
        self.vm.increaseHeapAllocationCount(size);
        object.set(variant.tag, self.vm.heap.protectedValue(item));
        const result = try self.vm.heap.protectValue(.{ .heap_obj = &object.obj }, .@"union");
        if (site_value) |found| {
            const tag = try self.vm.heap.protectValue(.{ .int64 = variant.tag }, .int64);
            try self.queueConversion(variant.proxy.?.decode_function, &.{ result, tag }, found);
        }
        self.path.shrinkRetainingCapacity(path_base);
        return result;
    }

    fn skipValue(self: *Decoder, depth: usize) anyerror!void {
        const kind = self.lexer.peekKind();
        switch (kind) {
            .object => {
                try self.enterContainer(.object, depth);
                var keys = std.StringHashMap(void).init(self.allocator);
                var is_first = true;
                while (try self.lexer.nextKey(&is_first)) |key_text| {
                    const entry = try keys.getOrPut(key_text);
                    if (entry.found_existing) {
                        try self.appendFieldPath(key_text, true);
                        return error.DuplicateKey;
                    }
                    const path_base = self.path.items.len;
                    try self.appendFieldPath(key_text, false);
                    try self.skipValue(depth + 1);
                    self.path.shrinkRetainingCapacity(path_base);
                }
            },
            .array => {
                try self.enterContainer(.array, depth);
                var is_first = true;
                var i: usize = 0;
                while (try self.lexer.hasItem(&is_first)) : (i += 1) {
                    const path_base = self.path.items.len;
                    try self.appendIndexPath(i);
                    try self.skipValue(depth + 1);
                    self.path.shrinkRetainingCapacity(path_base);
                }
            },
            .string => _ = try self.lexer.readString(),
            .number => _ = try self.lexer.readNumber(),
            .literal_true => try self.lexer.readLiteral("true"),
            .literal_false => try self.lexer.readLiteral("false"),
            .literal_null => try self.lexer.readLiteral("null"),
            .end, .invalid => return error.InvalidJson,
        }
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
        if (!self.hasDiagnostic()) return;
        const path = try self.vm.allocBytesValue(self.path.items, .string);
        try writeDiagnosticFields(self.vm, self.diagnostic, path, Position.at(self.source, self.lexer.token_start));
    }
};

const ValueKind = enum { object, array, string, number, literal_true, literal_false, literal_null, end, invalid };

const Number = struct {
    text: []const u8,
    is_integer: bool,
};

const Lexer = struct {
    source: []const u8,
    allocator: std.mem.Allocator,
    pos: usize = 0,
    token_start: usize = 0,

    fn peekKind(self: *Lexer) ValueKind {
        self.skipSpace();
        if (self.pos >= self.source.len) {
            return .end;
        }
        return switch (self.source[self.pos]) {
            '{' => .object,
            '[' => .array,
            '"' => .string,
            '-', '0'...'9' => .number,
            't' => .literal_true,
            'f' => .literal_false,
            'n' => .literal_null,
            else => .invalid,
        };
    }

    fn skipSpace(self: *Lexer) void {
        while (self.pos < self.source.len) : (self.pos += 1) {
            switch (self.source[self.pos]) {
                ' ', '\t', '\n', '\r' => {},
                else => break,
            }
        }
        self.token_start = self.pos;
    }

    fn nextKey(self: *Lexer, is_first: *bool) !?[]const u8 {
        self.skipSpace();
        if (self.pos >= self.source.len) {
            return error.InvalidJson;
        }
        if (self.source[self.pos] == '}') {
            self.pos += 1;
            return null;
        }
        if (!is_first.*) {
            if (self.source[self.pos] != ',') {
                return error.InvalidJson;
            }
            self.pos += 1;
            self.skipSpace();
        }
        is_first.* = false;
        const has_key = self.pos < self.source.len and self.source[self.pos] == '"';
        if (!has_key) {
            return error.InvalidJson;
        }
        const key = try self.readString();
        self.skipSpace();
        const has_colon = self.pos < self.source.len and self.source[self.pos] == ':';
        if (!has_colon) {
            return error.InvalidJson;
        }
        self.pos += 1;
        return key;
    }

    fn hasItem(self: *Lexer, is_first: *bool) !bool {
        self.skipSpace();
        if (self.pos >= self.source.len) {
            return error.InvalidJson;
        }
        if (self.source[self.pos] == ']') {
            self.pos += 1;
            return false;
        }
        if (!is_first.*) {
            if (self.source[self.pos] != ',') {
                return error.InvalidJson;
            }
            self.pos += 1;
        }
        is_first.* = false;
        return true;
    }

    fn readLiteral(self: *Lexer, word: []const u8) !void {
        const stop = self.pos + word.len;
        const is_match = stop <= self.source.len and std.mem.eql(u8, self.source[self.pos..stop], word);
        if (!is_match) {
            return error.InvalidJson;
        }
        self.pos = stop;
    }

    fn readNumber(self: *Lexer) !Number {
        const start = self.pos;
        if (self.source[self.pos] == '-') {
            self.pos += 1;
        }
        if (self.pos >= self.source.len) {
            return error.InvalidJson;
        }
        switch (self.source[self.pos]) {
            '0' => self.pos += 1,
            '1'...'9' => self.skipDigits(),
            else => return error.InvalidJson,
        }
        var is_integer = true;
        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            self.pos += 1;
            try self.requireDigits();
            is_integer = false;
        }
        const has_exponent = self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E');
        if (has_exponent) {
            self.pos += 1;
            const has_sign = self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-');
            if (has_sign) {
                self.pos += 1;
            }
            try self.requireDigits();
            is_integer = false;
        }
        return .{ .text = self.source[start..self.pos], .is_integer = is_integer };
    }

    fn skipDigits(self: *Lexer) void {
        while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
            self.pos += 1;
        }
    }

    fn requireDigits(self: *Lexer) !void {
        const start = self.pos;
        self.skipDigits();
        if (self.pos == start) {
            return error.InvalidJson;
        }
    }

    fn readString(self: *Lexer) ![]const u8 {
        self.pos += 1;
        const start = self.pos;
        var has_non_ascii = false;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '"') {
                const text = self.source[start..self.pos];
                self.pos += 1;
                if (has_non_ascii and !std.unicode.utf8ValidateSlice(text)) {
                    return error.InvalidJson;
                }
                return text;
            }
            if (c == '\\') {
                return self.readEscapedString(start);
            }
            if (c < 0x20) {
                return error.InvalidJson;
            }
            if (c >= 0x80) {
                has_non_ascii = true;
            }
            self.pos += 1;
        }
        return error.InvalidJson;
    }

    fn readEscapedString(self: *Lexer, start: usize) ![]const u8 {
        var out: std.ArrayList(u8) = .empty;
        try out.appendSlice(self.allocator, self.source[start..self.pos]);
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            self.pos += 1;
            if (c == '"') {
                if (!std.unicode.utf8ValidateSlice(out.items)) {
                    return error.InvalidJson;
                }
                return out.items;
            }
            if (c < 0x20) {
                return error.InvalidJson;
            }
            if (c != '\\') {
                try out.append(self.allocator, c);
                continue;
            }
            if (self.pos >= self.source.len) {
                return error.InvalidJson;
            }
            const escape = self.source[self.pos];
            self.pos += 1;
            switch (escape) {
                '"', '\\', '/' => try out.append(self.allocator, escape),
                'b' => try out.append(self.allocator, 0x08),
                'f' => try out.append(self.allocator, 0x0c),
                'n' => try out.append(self.allocator, '\n'),
                'r' => try out.append(self.allocator, '\r'),
                't' => try out.append(self.allocator, '\t'),
                'u' => try self.appendCodepoint(&out),
                else => return error.InvalidJson,
            }
        }
        return error.InvalidJson;
    }

    fn appendCodepoint(self: *Lexer, out: *std.ArrayList(u8)) !void {
        var codepoint: u21 = try self.readHex4();
        if (codepoint >= 0xDC00 and codepoint <= 0xDFFF) {
            return error.InvalidJson;
        }
        if (codepoint >= 0xD800 and codepoint <= 0xDBFF) {
            const has_pair = self.pos + 1 < self.source.len and self.source[self.pos] == '\\' and self.source[self.pos + 1] == 'u';
            if (!has_pair) {
                return error.InvalidJson;
            }
            self.pos += 2;
            const low = try self.readHex4();
            if (low < 0xDC00 or low > 0xDFFF) {
                return error.InvalidJson;
            }
            codepoint = 0x10000 + ((codepoint - 0xD800) << 10) + (low - 0xDC00);
        }
        var buffer: [4]u8 = undefined;
        const length = std.unicode.utf8Encode(codepoint, &buffer) catch return error.InvalidJson;
        try out.appendSlice(self.allocator, buffer[0..length]);
    }

    fn readHex4(self: *Lexer) !u21 {
        if (self.pos + 4 > self.source.len) {
            return error.InvalidJson;
        }
        var value: u21 = 0;
        for (self.source[self.pos .. self.pos + 4]) |c| {
            const digit = std.fmt.charToDigit(c, 16) catch return error.InvalidJson;
            value = value * 16 + digit;
        }
        self.pos += 4;
        return value;
    }
};

const Position = struct {
    offset: i64,
    line: i64,
    column: i64,

    fn at(source: []const u8, offset: usize) Position {
        const stop = @min(offset, source.len);
        var line: i64 = 1;
        var column: i64 = 1;
        for (source[0..stop]) |c| {
            if (c == '\n') {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        return .{ .offset = @intCast(stop), .line = line, .column = column };
    }
};

fn writeDiagnosticFields(vm: *VM, diagnostic: usize, path: v.Value, position: Position) !void {
    const path_root = try vm.heap.protectValue(path, .string);
    v.HeapValue.getParentPtr(v.Struct, vm.heap.protectedValue(diagnostic).heap_obj).set(0, vm.heap.protectedValue(path_root));
    const positions = [_]i64{ position.offset, position.line, position.column };
    for (positions, 1..) |number, i| {
        const boxed = try vm.allocNullBox(.{ .int64 = number }, .int64);
        v.HeapValue.getParentPtr(v.Struct, vm.heap.protectedValue(diagnostic).heap_obj).set(i, boxed);
    }
}
