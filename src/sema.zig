pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    current_scope: *SymbolTable,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .allocator = allocator, .current_scope = try SymbolTable.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        var next = self.current_scope;
        while (next != null) {
            next = self.current_scope.deinit();
        }
    }

    pub fn analyze(self: *Self, root: *ast.Root) !void {}

    pub fn anaylizeExpression(_: *Self, exp: *ast.Exp) !Type {
        switch (exp) {
            .number_literal => {
                return .number;
            },
            .string_literal => {
                return .string;
            },
            .bool_literal => {
                return .boolean;
            },
            else => unreachable,
        }
    }

    pub fn visitLetStmt(self: *Self, letStmt: *ast.LetStmt) !void {
        const expression_type = try self.anaylizeExpression(letStmt.value);
        const var_type = Type.fromString(letStmt.type_annotation.name);

        if (expression_type != var_type) {
            return SemaError.InvalidExpressionType;
        }

        self.current_scope.put(.{ .id = letStmt.identifier, .type = var_type });
    }

    pub fn visitIdentifier(self: *Self, id: []const u8) !Type {
        return self.current_scope.getOrThrow(id);
    }

    pub fn visitBinaryExp(self: *Self, bin_exp: *ast.BinaryOp) {
        
    }

    pub fn enterScope(self: *Self) !void {
        self.current_scope = try SymbolTable.init(self.allocator, self.current_scope);
    }

    pub fn exitScope(self: *Self) !void {
        if (self.current_scope.parent) |parent_scope| {
            self.current_scope = parent_scope;
        }
    }
};

pub const SymbolTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable) !*Self {
        const self = try allocator.create(Self);
        self.* = Self{ .allocator = allocator, .parent = parent, .symbols = std.StringHashMap(Symbol).init(allocator) };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.id)) |_| {
            return SemaError.AlreadyDefined;
        }

        try self.symbols.put(symbol.id, symbol);
    }

    pub fn get(self: *Self, id: []const u8) ?Symbol {
        if (self.symbols.get(id)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(id);
        }
        return null;
    }

    pub fn getOrThrow(self: *Self, id: []const u8) !Symbol {
        if (self.get(id)) |s| return s;
        return SemaError.NotDefined;
    }
};

pub const Symbol = struct {
    id: []const u8,
    type: Type,
    is_mut: bool,
};

pub const Type = enum {
    number,
    string,
    boolean,
    function,
    void,
    unknown,

    pub fn fromString(name: []const u8) Type {
        if (std.mem.eql(u8, name, "number")) return .number;
        if (std.mem.eql(u8, name, "bool")) return .boolean;
        if (std.mem.eql(u8, name, "string")) return .string;
        if (std.mem.eql(u8, name, "fn")) return .function;
        if (std.mem.eql(u8, name, "void")) return .void;
        return .unknown;
    }
};

pub const SemaError = error{
    AlreadyDefined,
    NotDefined,
    InvalidExpressionType,
    OutOfMemory,
};

const std = @import("std");
const ast = @import("ast.zig");
// const lexer = @import("lexer.zig");
// const parser = @import("parser.zig");
// const Token = lexer.Token;
// const Tag = lexer.Tag;
