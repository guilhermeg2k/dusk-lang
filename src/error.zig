pub const Loc = @import("lexer.zig").Loc;

pub const CompileErrorReporter = struct {
    const Self = @This();

    src: []const u8,
    allocator: std.mem.Allocator,
    copy_context: ?CopyContext = null,

    const LINE_MAX_CHAR_COUNT = 256;
    const padding = " " ** LINE_MAX_CHAR_COUNT;

    pub fn unexpectedToken(self: *Self, token: Token) CompileError {
        self.log(try allocPrint(self.allocator, "Unexpected token '{s}'", .{token.value(self.src)}), token.loc);
        return CompileError.LexerError;
    }

    pub fn invalidSyntax(self: *Self, expected: []const u8, token: Token) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid Syntax: expected {s} found '{s}'",
                .{ expected, token.value(self.src) },
            ),
            token.loc,
        );
        return CompileError.ParserError;
    }

    pub fn invalidType(self: *Self, expected: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid type: expected '{s}' found '{s}'",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidType;
    }

    pub fn stringIndexing(self: *Self, loc: Loc) CompileError {
        self.log("Strings can't be indexed: use str_byte_at(s, i) or str_char_at(s, i)", loc);
        return error.InvalidIndexing;
    }

    pub fn byteOutOfRange(self: *Self, value: i64, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Value {d} doesn't fit in byte (0..255)",
                .{value},
            ),
            loc,
        );
        return error.ByteOutOfRange;
    }

    pub fn invalidFunctionReturnType(self: *Self, expected: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid function return type: expected '{s}' found '{s}'",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidReturnType;
    }

    pub fn invalidExpression(self: *Self, expected: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid expression: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidExpression;
    }

    pub fn invalidIndexing(self: *Self, expected: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid indexing: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidIndexing;
    }

    pub fn invalidAssignment(self: *Self, expected: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid assignment: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidAssignment;
    }

    pub fn typeNotDefined(self: *Self, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(self.allocator, "Invalid type '{s}'", .{found}),
            loc,
        );
        return error.UnknownType;
    }

    pub fn invalidNumberOfArgs(self: *Self, expected: usize, found: usize, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid number of arguments: expected {d} found {d}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidNumberOfArgs;
    }

    pub fn missingArgument(self: *Self, expected: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Missing argument '{s}'",
                .{expected},
            ),
            loc,
        );
        return error.MissingArgument;
    }

    pub fn alreadyDefined(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is already defined", .{identifier}), loc);
        return error.AlreadyDefined;
    }

    pub fn notDefined(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Cannot find name '{s}'", .{identifier}), loc);
        return error.NotDefined;
    }

    pub fn notMutable(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is not mutable", .{identifier}), loc);
        return error.NotMutable;
    }


    pub fn unwrappedValueCantBeMutable(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Unwrapped value '{s}' cannot be mutable", .{identifier}), loc);
        return error.UnwrappedCantBeMutable;
    }

    pub fn selfCantBeUsedOutsideOfAstruct(self: *Self, loc: Loc) CompileError {
        self.log("@ cannot be used outside of a struct", loc);
        return error.SelfOutsideStruct;
    }

    pub fn invalidStaticStructField(self: *Self, struct_name: []const u8, field: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is not a static field of struct '{s}'",
                .{ field, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidStructField(self: *Self, struct_name: []const u8, member: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' does not exists on struct '{s}'",
                .{ member, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidStructFunction(self: *Self, struct_name: []const u8, function: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Function '{s}' does not exists on struct '{s}'",
                .{ function, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidEnumFunction(self: *Self, enum_name: []const u8, function: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Function '{s}' does not exists on enum '{s}'",
                .{ function, enum_name },
            ),
            loc,
        );

        return error.InvalidEnumFunction;
    }

    pub fn invalidEnumVariant(self: *Self, enum_name: []const u8, variant: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "enum '{s}' has no variant '{s}'",
                .{ enum_name, variant },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidUnionVariant(self: *Self, union_name: []const u8, variant: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "union '{s}' has no variant '{s}'",
                .{ union_name, variant },
            ),
            loc,
        );
        return error.InvalidUnionVariant;
    }

    pub fn invalidUnionMember(self: *Self, union_name: []const u8, member: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "union '{s}' has no variant, static field or function '{s}'",
                .{ union_name, member },
            ),
            loc,
        );
        return error.InvalidUnionMember;
    }

    pub fn invalidUnionFunction(self: *Self, union_name: []const u8, function: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Function '{s}' does not exists on union '{s}'",
                .{ function, union_name },
            ),
            loc,
        );

        return error.InvalidUnionFunction;
    }

    pub fn tooManyUnionVariants(self: *Self, union_name: []const u8, max: usize, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "union '{s}' has too many variants: the limit is {d}",
                .{ union_name, max },
            ),
            loc,
        );
        return error.TooManyUnionVariants;
    }

    pub fn whenNotExhaustive(self: *Self, missing: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "when is not exhaustive, missing: {s}",
                .{missing},
            ),
            loc,
        );
        return error.WhenNotExhaustive;
    }

    pub fn whenNeedsElse(self: *Self, subject_type: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "when on '{s}' must have an else case",
                .{subject_type},
            ),
            loc,
        );
        return error.WhenNotExhaustive;
    }

    pub fn whenUselessElse(self: *Self, loc: Loc) CompileError {
        self.log("else is never reached: every case is already handled", loc);
        return error.WhenUselessElse;
    }

    pub fn whenDuplicateCase(self: *Self, case: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "case '{s}' is already handled",
                .{case},
            ),
            loc,
        );
        return error.WhenDuplicateCase;
    }

    pub fn cantCompareTypes(self: *Self, op: []const u8, left: []const u8, right: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare '{s}' with '{s}': values of different types are never equal", .{ op, left, right }), loc);
        return error.CantCompare;
    }

    pub fn cantCompareWithNull(self: *Self, op: []const u8, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare '{s}' with null: only nullable values can be null", .{ op, type_name }), loc);
        return error.CantCompare;
    }

    pub fn cantCompareStrings(self: *Self, op: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare strings: use str_equal(a, b) to compare their text", .{op}), loc);
        return error.CantCompare;
    }

    pub fn cantCompareUnions(self: *Self, op: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare unions: check the variant with 'when' or 'if u.variant: value then ... end'", .{op}), loc);
        return error.CantCompare;
    }

    pub fn whenInvalidCase(self: *Self, msg: []const u8, loc: Loc) CompileError {
        self.log(msg, loc);
        return error.WhenInvalidCase;
    }

    pub fn unhandledError(self: *Self, function: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' can fail: handle the error with try or catch",
                .{function},
            ),
            loc,
        );
        return error.UnhandledError;
    }

    pub fn tryOutsideFailingFunction(self: *Self, loc: Loc) CompileError {
        self.log("try can only be used at the top level or in a function that can fail (-> E!T or -> !T)", loc);
        return error.TryOutsideFailingFunction;
    }

    pub fn notAFailingCall(self: *Self, keyword: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "{s} needs a call to a function that can fail",
                .{keyword},
            ),
            loc,
        );
        return error.NotAFailingCall;
    }

    pub fn topLevelLetNotConstant(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Top-level 'let {s}' must be a constant: a 'let' (not 'mut') with an int, float, bool, byte, string or enum value known at compile time. Declare variables inside 'main'",
                .{name},
            ),
            loc,
        );
        return error.TopLevelLetNotConstant;
    }

    pub fn statementOutsideMain(self: *Self, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Statements must be inside a function: the top level only holds declarations and constants. Move this into 'main'", .{}), loc);
        return error.StatementOutsideMain;
    }

    pub fn declarationInsideFunction(self: *Self, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Functions, types and error sets must be declared at the top level, not inside a function", .{}), loc);
        return error.DeclarationInsideFunction;
    }

    pub fn externOutsidePrelude(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'extern func {s}' is only allowed in the prelude", .{identifier}), loc);
        return error.ExternOutsidePrelude;
    }

    pub fn slotParamCantHaveDefault(self: *Self, param: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Parameter '{s}' uses a type slot, so it can't have a default value", .{param}), loc);
        return error.SlotParamCantHaveDefault;
    }

    pub fn slotOperatorNotAllowed(self: *Self, operator: []const u8, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' isn't allowed on type slot '{s}': it needs a type list constraint", .{ operator, slot }), loc);
        return error.SlotOperatorNotAllowed;
    }

    pub fn slotOperatorNotAllowedOn(self: *Self, operator: []const u8, slot: []const u8, member: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' isn't allowed on '{s}': '{s}' doesn't support it", .{ operator, slot, member }), loc);
        return error.SlotOperatorNotAllowed;
    }

    pub fn slotStringEquality(self: *Self, operator: []const u8, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' isn't allowed on '{s}': on a string it would compare pointers. Use str_equal", .{ operator, slot }), loc);
        return error.SlotOperatorNotAllowed;
    }

    pub fn slotResultDiffers(self: *Self, operator: []const u8, first_member: []const u8, first_result: []const u8, second_member: []const u8, second_result: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' has no single result type here: it gives '{s}' for '{s}' and '{s}' for '{s}'", .{ operator, first_result, first_member, second_result, second_member }), loc);
        return error.SlotResultDiffers;
    }

    pub fn typeDoesntFitSlot(self: *Self, type_name: []const u8, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "{s} doesn't fit {s}", .{ type_name, slot }), loc);
        return error.TypeDoesntFitSlot;
    }

    pub fn typeListNotPrimitive(self: *Self, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Type lists only hold int, float, byte, string and bool, found '{s}'", .{type_name}), loc);
        return error.TypeListNotPrimitive;
    }

    pub fn traitRefersToItself(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Trait '{s}' refers to itself", .{identifier}), loc);
        return error.TraitRefersToItself;
    }

    pub fn doesntFitTrait(self: *Self, type_name: []const u8, slot: []const u8, reason: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "{s} doesn't fit {s}: {s}", .{ type_name, slot, reason }), loc);
        return error.DoesntFitTrait;
    }

    pub fn typeListWithTrait(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' mixes a type list with another constraint: '+' only joins traits with methods, and primitives have no methods", .{identifier}), loc);
        return error.TypeListWithTrait;
    }

    pub fn notATrait(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' isn't a trait", .{identifier}), loc);
        return error.NotATrait;
    }

    pub fn clashingTraitMethods(self: *Self, method: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Method '{s}' comes from two traits with different signatures", .{method}), loc);
        return error.ClashingTraitMethods;
    }

    pub fn traitMethodNeedsSelf(self: *Self, method: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Trait method '{s}' must take 'self: @' or 'mut self: @' first", .{method}), loc);
        return error.TraitMethodNeedsSelf;
    }

    pub fn traitMethodNeedsErrorSet(self: *Self, method: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Trait method '{s}' needs a named error set, like 'ParseError!int'", .{method}), loc);
        return error.TraitMethodNeedsErrorSet;
    }

    pub fn slotHasNoMethod(self: *Self, slot: []const u8, method: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' has no method '{s}': its constraint doesn't list it", .{ slot, method }), loc);
        return error.SlotHasNoMethod;
    }

    pub fn traitUsedAsType(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is a trait: it can only constrain a type slot, like [T: {s}]", .{ identifier, identifier }), loc);
        return error.TraitUsedAsType;
    }

    pub fn internalBuiltin(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is internal to the prelude and can't be used directly", .{identifier}), loc);
        return error.InternalBuiltin;
    }

    pub fn slotUsedAsValue(self: *Self, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is a type slot, not a value", .{slot}), loc);
        return error.SlotUsedAsValue;
    }

    pub fn genericNeedsCall(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Generic function '{s}' can only be called, not used as a value", .{identifier}), loc);
        return error.GenericNeedsCall;
    }

    pub fn cantInferSlot(self: *Self, slot: []const u8, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't work out '{s}' for '{s}': pass it through an argument or give the result a type", .{ slot, identifier }), loc);
        return error.CantInferSlot;
    }

    pub fn conflictingSlotTypes(self: *Self, slot: []const u8, first: []const u8, second: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't be both '{s}' and '{s}'", .{ slot, first, second }), loc);
        return error.ConflictingSlotTypes;
    }

    pub fn invalidSlotType(self: *Self, slot: []const u8, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't be '{s}'", .{ slot, type_name }), loc);
        return error.InvalidSlotType;
    }

    pub fn wrongSlotCount(self: *Self, identifier: []const u8, expected: usize, found: usize, loc: Loc) CompileError {
        if (expected == 0) {
            self.log(try allocPrint(self.allocator, "'{s}' isn't generic, so it takes no type arguments", .{identifier}), loc);
            return error.WrongSlotCount;
        }
        self.log(try allocPrint(self.allocator, "'{s}' takes {d} type argument(s), found {d}", .{ identifier, expected, found }), loc);
        return error.WrongSlotCount;
    }

    pub fn genericCopyTooDeep(self: *Self, name: []const u8, max_depth: usize, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' needs copies nested more than {d} deep: it probably calls itself with a bigger type each time", .{ name, max_depth }), loc);
        return error.GenericCopyTooDeep;
    }

    pub fn missingMain(self: *Self) CompileError {
        self.log(try allocPrint(self.allocator, "Missing 'main': declare 'func main() -> void' (or '-> !void')", .{}), .{ .start = 0, .end = 0 });
        return error.MissingMain;
    }

    pub fn invalidMainSignature(self: *Self, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'main' must take no parameters and return 'void' or an error union of 'void' ('-> !void')", .{}), loc);
        return error.InvalidMainSignature;
    }

    pub fn staticFieldCantBeMutable(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Static field '{s}' can't be mut: static fields are constants", .{name}), loc);
        return error.StaticFieldCantBeMutable;
    }

    pub fn staticFieldNotConstant(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Static field '{s}' must be a constant: an int, float, bool, byte, string or enum value known at compile time",
                .{name},
            ),
            loc,
        );
        return error.StaticFieldNotConstant;
    }

    pub fn invalidConstantType(self: *Self, name: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Constant '{s}' must be an int, float, bool, byte, string or enum, found '{s}'",
                .{ name, found },
            ),
            loc,
        );
        return error.InvalidConstantType;
    }

    pub fn payloadSharesErrors(self: *Self, payload_name: []const u8, set_name: []const u8, error_name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "success type {s} shares '{s}' with {s}",
                .{ payload_name, error_name, set_name },
            ),
            loc,
        );
        return error.PayloadSharesErrors;
    }

    pub fn errorNotInSet(self: *Self, error_name: []const u8, set_name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is not in {s}",
                .{ error_name, set_name },
            ),
            loc,
        );
        return error.ErrorNotInSet;
    }

    pub fn catchBlockMustLeave(self: *Self, loc: Loc) CompileError {
        self.log("catch block must return, break or continue when the value is used", loc);
        return error.CatchBlockMustLeave;
    }

    pub fn notAnErrorSet(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is not an error set",
                .{name},
            ),
            loc,
        );
        return error.NotAnErrorSet;
    }

    pub fn invalidDefinition(self: *Self, loc: Loc) CompileError {
        self.log("struct and enum definitions cannot be mutable", loc);
        return error.InvalidDefinition;
    }

    pub fn cantInferAnonymousStruct(self: *Self, loc: Loc) CompileError {
        self.log(
            "Can't infer anonymous struct type",
            loc,
        );
        return error.CantInferAnonymousStruct;
    }

    pub fn primitiveParamsCantBeMutable(self: *Self, loc: Loc) CompileError {
        self.log(
            "Primitive params can't be mutable: only arrays, buffers, structs and unions can be changed through a mut param",
            loc,
        );
        return error.PrimitiveParamsCantBeMutable;
    }

    pub fn readOnlyIntoMutable(self: *Self, text: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(self.allocator, "'{s}' can't change, so it can't go into a mutable value: use copy({s}), or make it mutable", .{ text, text }),
            loc,
        );
        return error.ReadOnlyIntoMutable;
    }

    pub fn primitiveCaptureCantBeMutable(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Capture '{s}' can't be mut: only arrays, buffers, structs and unions can be changed through a mut capture",
                .{name},
            ),
            loc,
        );
        return error.PrimitiveCaptureCantBeMutable;
    }

    pub fn mutBorrowCantBeReassigned(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is a mut param or capture: it can't be reassigned, only changed through it (items, fields, append)",
                .{name},
            ),
            loc,
        );
        return error.MutBorrowCantBeReassigned;
    }

    pub fn cantInferArrayLiteralType(self: *Self, loc: Loc) CompileError {
        self.log(
            "Can't infer array literal type",
            loc,
        );
        return error.CantInferArrayLiteralType;
    }

    pub fn unnecessaryOptionalChain(self: *Self, member: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Unecessary optional chain for '{s}'",
                .{member},
            ),
            loc,
        );
        return error.UnecessaryOptionalChain;
    }

    pub fn nullableMustBeUnwraped(self: *Self, member: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "{s} must be unwrapped",
                .{member},
            ),
            loc,
        );
        return error.NullableMustBeUnwrapped;
    }

    pub fn invalidParameterType(self: *Self, parameter_name: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "Parameter '{s}' must have a type",
                .{parameter_name},
            ),
            loc,
        );
        return error.InvalidParameterType;
    }

    pub fn log(self: *Self, message: []const u8, loc: Loc) void {
        if (@import("builtin").is_test) return;
        const msg = self.withCopyContext(message);

        const exactLoc = self.findExactLoc(loc.start);
        const start_col = 6 + loc.start - exactLoc.line.start;

        const capped_end = @min(loc.end, exactLoc.line.end);
        const range_len = if (capped_end > loc.start) (capped_end - loc.start) else 0;

        var error_loc_marker: [LINE_MAX_CHAR_COUNT]u8 = undefined;
        var error_loc_marker_len: usize = 0;

        const white_space_count = @min(start_col, LINE_MAX_CHAR_COUNT);
        @memcpy(error_loc_marker[0..white_space_count], padding[0..white_space_count]);
        error_loc_marker_len += white_space_count;

        if (error_loc_marker_len < LINE_MAX_CHAR_COUNT) {
            error_loc_marker[error_loc_marker_len] = '^';
            error_loc_marker_len += 1;
        }

        if (range_len > 0) {
            const remaining_space = if (LINE_MAX_CHAR_COUNT > error_loc_marker_len + 1) LINE_MAX_CHAR_COUNT - error_loc_marker_len - 1 else 0;
            const tilde_count = @min(range_len, remaining_space);

            if (tilde_count > 0) {
                @memset(error_loc_marker[error_loc_marker_len .. error_loc_marker_len + tilde_count], '~');
                error_loc_marker_len += tilde_count;
            }

            if (error_loc_marker_len < LINE_MAX_CHAR_COUNT) {
                error_loc_marker[error_loc_marker_len] = '^';
                error_loc_marker_len += 1;
            }
        }

        std.log.err(
            "{s} at line {d} col {d}\n--> {d}: {s}\n {s}",
            .{
                msg,
                exactLoc.line.count,
                exactLoc.col,
                exactLoc.line.count,
                std.mem.trimEnd(u8, self.src[@min(exactLoc.line.start, self.src.len)..@min(exactLoc.line.end + 1, self.src.len)], "\r\n"),
                error_loc_marker[0..error_loc_marker_len],
            },
        );
    }

    fn withCopyContext(self: *Self, message: []const u8) []const u8 {
        const context = self.copy_context orelse return message;
        const saved_src = self.src;
        self.src = context.call_src;
        const line = self.findExactLoc(context.call_loc.start).line.count;
        self.src = saved_src;
        return allocPrint(self.allocator, "in {s} (created at line {d}): {s}", .{ context.name, line, message }) catch message;
    }

    fn findExactLoc(self: *Self, loc: usize) ExactLoc {
        var line_count: usize = 1;
        var line_start: usize = 0;
        var line_end: usize = self.src.len -| 1;

        var i: usize = 0;
        var should_break_in_next_line = false;
        while (i < self.src.len) {
            if (i == loc) should_break_in_next_line = true;
            if (self.src[i] == '\n') {
                if (should_break_in_next_line) {
                    line_end = i;
                    break;
                }

                line_start = i + 1;
                line_count += 1;
            }
            i += 1;
        }

        var col_count: usize = 1;
        i = line_start;
        while (true) {
            if (i == loc) break;
            col_count += 1;
            i += 1;
        }

        return .{
            .line = .{
                .count = line_count,
                .start = line_start,
                .end = line_end,
            },
            .col = col_count,
        };
    }
};

const ExactLoc = struct { line: struct {
    count: usize,
    start: usize,
    end: usize,
}, col: usize };

pub const CopyContext = struct {
    name: []const u8,
    call_loc: Loc,
    call_src: []const u8,
};

pub const CompileError = error{
    OutOfMemory,
    UnableToFindLoc,
    AstError,
    LexerError,
    ParserError,
    SemaError,
    InvalidType,
    ByteOutOfRange,
    InvalidReturnType,
    InvalidExpression,
    InvalidIndexing,
    InvalidAssignment,
    UnknownType,
    InvalidNumberOfArgs,
    MissingArgument,
    AlreadyDefined,
    NotDefined,
    NotMutable,
    ReadOnlyIntoMutable,
    UnwrappedCantBeMutable,
    SelfOutsideStruct,
    InvalidStructField,
    InvalidDefinition,
    CantInferAnonymousStruct,
    PrimitiveParamsCantBeMutable,
    CantInferArrayLiteralType,
    UnecessaryOptionalChain,
    NullableMustBeUnwrapped,
    InvalidParameterType,
    InvalidEnumFunction,
    InvalidUnionVariant,
    InvalidUnionMember,
    InvalidUnionFunction,
    PrimitiveCaptureCantBeMutable,
    MutBorrowCantBeReassigned,
    TooManyUnionVariants,
    WhenNotExhaustive,
    WhenUselessElse,
    WhenDuplicateCase,
    WhenInvalidCase,
    CantCompare,
    UnhandledError,
    TryOutsideFailingFunction,
    NotAFailingCall,
    ErrorNotInSet,
    PayloadSharesErrors,
    TopLevelLetNotConstant,
    StatementOutsideMain,
    MissingMain,
    DeclarationInsideFunction,
    ExternOutsidePrelude,
    SlotParamCantHaveDefault,
    SlotOperatorNotAllowed,
    SlotUsedAsValue,
    InternalBuiltin,
    SlotResultDiffers,
    TypeDoesntFitSlot,
    TypeListNotPrimitive,
    TraitRefersToItself,
    TraitUsedAsType,
    DoesntFitTrait,
    TypeListWithTrait,
    NotATrait,
    ClashingTraitMethods,
    TraitMethodNeedsSelf,
    TraitMethodNeedsErrorSet,
    SlotHasNoMethod,
    GenericNeedsCall,
    CantInferSlot,
    ConflictingSlotTypes,
    InvalidSlotType,
    GenericCopyTooDeep,
    WrongSlotCount,
    InvalidMainSignature,
    StaticFieldCantBeMutable,
    StaticFieldNotConstant,
    InvalidConstantType,
    CatchBlockMustLeave,
    NotAnErrorSet,
};

pub const RuntimeError = error{
    OutOfMemory,
    ArrayOutOfBounds,
    ByteOutOfRange,
    IndexOutOfBounds,
    NegativeBufferSize,
    UnhandledError,
    CallDepthExceeded,
    DivisionByZero,
    DivisionOverflow,
    IntegerOverflow,
    FloatOutOfIntRange,
};

pub const RuntimeErrorReporter = struct {
    pub noinline fn arrayOutOfBounds(index: i64, len: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.ArrayOutOfBounds, "index {d} is out of bounds for array of length {d}", .{ index, len });
    }

    pub noinline fn indexOutOfBounds(index: i64, len: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IndexOutOfBounds, "index {d} is out of bounds for length {d}", .{ index, len });
    }

    pub noinline fn negativeBufferSize(len: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.NegativeBufferSize, "buffer size can't be negative: {d}", .{len});
    }

    pub noinline fn byteOutOfRange(number: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.ByteOutOfRange, "value {d} doesn't fit in byte (0..255)", .{number});
    }

    pub noinline fn callDepthExceeded(max_depth: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.CallDepthExceeded, "call depth exceeded: more than {d} nested calls", .{max_depth});
    }

    pub noinline fn divisionByZero(operator: []const u8) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.DivisionByZero, "integer division by zero ('{s}' with a right side of 0)", .{operator});
    }

    pub noinline fn divisionOverflow(dividend: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.DivisionOverflow, "integer overflow: {d} // -1 doesn't fit in an int", .{dividend});
    }

    pub noinline fn integerOverflow(operator: []const u8, left: i64, right: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IntegerOverflow, "integer overflow: {d} {s} {d} doesn't fit in an int", .{ left, operator, right });
    }

    pub noinline fn negationOverflow(operand: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IntegerOverflow, "integer overflow: -({d}) doesn't fit in an int", .{operand});
    }

    pub noinline fn floatOutOfIntRange(value: f64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.FloatOutOfIntRange, "{d} doesn't fit in an int", .{value});
    }

    pub noinline fn unhandledError(error_name: []const u8) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.UnhandledError, "unhandled error {s}", .{error_name});
    }

    fn report(err: RuntimeError, comptime fmt: []const u8, args: anytype) RuntimeError {
        if (@import("builtin").is_test) return err;
        std.debug.print("error: " ++ fmt ++ "\n", args);
        return err;
    }
};

const std = @import("std");
const allocPrint = std.fmt.allocPrint;
const Token = @import("lexer.zig").Token;
