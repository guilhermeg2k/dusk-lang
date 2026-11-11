pub const Loc = @import("lexer.zig").Loc;

pub const CompileErrorReporter = struct {
    const Self = @This();

    src: []const u8,
    allocator: std.mem.Allocator,
    copy_context: ?CopyContext = null,
    file_path: ?[]const u8 = null,
    imported_modules: []const []const u8 = &.{},
    hidden_names: ?*const std.StringHashMapUnmanaged(void) = null,

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

    pub fn librariesNotSupported(self: *Self, path: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't import '{s}': libraries aren't supported yet", .{path}), loc);
        return error.LibrariesNotSupported;
    }

    pub fn unknownStdModule(self: *Self, path: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'std/{s}' isn't a standard library module", .{path}), loc);
        return error.UnknownStdModule;
    }

    pub fn moduleNotFound(self: *Self, path: []const u8, file_path: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Module '{s}' not found: there's no file '{s}'", .{ path, file_path }), loc);
        return error.ModuleNotFound;
    }

    pub fn cantReadModule(self: *Self, path: []const u8, reason: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't read module '{s}': {s}", .{ path, reason }), loc);
        return error.CantReadModule;
    }

    pub fn importCycle(self: *Self, chain: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Import cycle: {s}", .{chain}), loc);
        return error.ImportCycle;
    }

    fn checkModuleImported(self: *Self, name: []const u8, loc: Loc) CompileError!void {
        const separator = std.mem.indexOf(u8, name, "::") orelse return;
        const module = name[0..separator];
        const is_hidden = if (self.hidden_names) |hidden| hidden.contains(name) else false;
        if (is_hidden) {
            return self.notVisible(name, loc);
        }
        for (self.imported_modules) |imported| {
            if (std.mem.eql(u8, imported, module)) return;
        }
        self.log(try allocPrint(self.allocator, "Module '{s}' isn't imported: add an import for it at the top of the file", .{module}), loc);
        return error.ModuleNotImported;
    }

    pub fn notDefinedInModule(self: *Self, name: []const u8, module_path: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' isn't declared in module '{s}'", .{ name, module_path }), loc);
        return error.NotDefined;
    }

        pub fn importNotAtTop(self: *Self, loc: Loc) CompileError {
        self.log("Imports must come first in the file, before any other declaration", loc);
        return CompileError.ParserError;
    }

    pub fn stdNeedsModule(self: *Self, loc: Loc) CompileError {
        self.log("'std' is the standard library, not a module: import one of its modules, like 'std/string'", loc);
        return CompileError.ParserError;
    }

    pub fn fileOnlyNotAllowed(self: *Self, reason: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'~' can't be used here: {s}", .{reason}), loc);
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
        self.log("Strings can't be indexed: use string::byte_at(s, i) or string::char_at(s, i) from std/string", loc);
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
        try self.checkModuleImported(found, loc);
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
        try self.checkModuleImported(identifier, loc);
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

    pub fn invalidStaticUnionField(self: *Self, union_name: []const u8, field: []const u8, loc: Loc) CompileError {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is not a static field of union '{s}'",
                .{ field, union_name },
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

    pub fn cantCompareUnions(self: *Self, op: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare unions: check the variant with 'when' or 'if u.variant: value then ... end'", .{op}), loc);
        return error.CantCompare;
    }

    pub fn cantCompareFunctions(self: *Self, op: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare functions", .{op}), loc);
        return error.CantCompare;
    }

    pub fn unawaitedTaskCanFail(self: *Self, loc: Loc) CompileError {
        self.log("this task's result isn't used, so it can't fail: handle its errors inside it with catch", loc);
        return error.UnawaitedTaskCanFail;
    }

    pub fn taskNeedsSpawn(self: *Self, loc: Loc) CompileError {
        self.log("'Task' can't be created directly: start a task with 'spawn'", loc);
        return error.TaskNeedsSpawn;
    }

    pub fn cantCompareTasks(self: *Self, op: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't compare tasks", .{op}), loc);
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

    pub fn externOutsideStd(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'extern func {s}' is only allowed in the prelude and the standard library", .{identifier}), loc);
        return error.ExternOutsideStd;
    }

    pub fn externWithoutBinding(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'extern func {s}' has no native implementation", .{identifier}), loc);
        return error.ExternWithoutBinding;
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

    pub fn notVisible(self: *Self, name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is file-only: it's declared with '~' and can only be used in its own file", .{name}), loc);
        return error.NotVisible;
    }

    pub fn memberNotVisible(self: *Self, member: []const u8, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is a file-only member of '{s}': it can only be used in the file that declares '{s}'", .{ member, type_name, type_name }), loc);
        return error.NotVisible;
    }

    pub fn cantBuildOutsideModule(self: *Self, type_name: []const u8, field: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't build '{s}' outside its file: its file-only field '{s}' has no default value", .{ type_name, field }), loc);
        return error.CantBuildOutsideModule;
    }

    pub fn privateTypeInPublicSignature(self: *Self, type_name: []const u8, declaration: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is file-only, so the public '{s}' can't use it: mark '{s}' with '~' too, or remove the '~' from '{s}'", .{ type_name, declaration, declaration, type_name }), loc);
        return error.PrivateTypeInPublicSignature;
    }

    pub fn slotUsedAsValue(self: *Self, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is a type slot, not a value", .{slot}), loc);
        return error.SlotUsedAsValue;
    }

    pub fn genericNeedsCall(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Generic function '{s}' needs a function type to be used as a value, like 'let f: func(int) -> int = {s}'", .{ identifier, identifier }), loc);
        return error.GenericNeedsCall;
    }

    pub fn fieldAndMethodSameName(self: *Self, name: []const u8, struct_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is both a field and a method of '{s}': rename one of them", .{ name, struct_name }), loc);
        return error.FieldAndMethodSameName;
    }

    pub fn lambdaParamCantHaveDefault(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Lambda param '{s}' can't have a default value: a lambda is only called through a function value, which passes every argument", .{identifier}), loc);
        return error.LambdaParamCantHaveDefault;
    }

    pub fn lambdaCallsItself(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "A lambda can't use '{s}', the name it's being assigned to: declare '{s}' as a top-level function to call it recursively", .{ identifier, identifier }), loc);
        return error.LambdaCallsItself;
    }

    pub fn lambdaParamNeedsType(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't tell the type of lambda param '{s}': give it a type, or pass the lambda where a function type is expected", .{identifier}), loc);
        return error.LambdaParamNeedsType;
    }

    pub fn lambdaReturnsDiffer(self: *Self, first: []const u8, found: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "This lambda returns '{s}' here but '{s}' before: every return must have the same type, or give the lambda a return type", .{ found, first }), loc);
        return error.LambdaReturnsDiffer;
    }

    pub fn wrongLambdaParamCount(self: *Self, found: usize, expected: usize, loc: Loc) CompileError {
        const noun = if (found == 1) "param" else "params";
        self.log(try allocPrint(self.allocator, "This lambda takes {d} {s}, but the function type it's used as takes {d}", .{ found, noun, expected }), loc);
        return error.WrongLambdaParamCount;
    }

    pub fn lambdaParamMutDiffers(self: *Self, identifier: []const u8, is_expected_mut: bool, loc: Loc) CompileError {
        const text = if (is_expected_mut)
            try allocPrint(self.allocator, "Lambda param '{s}' needs 'mut': the function type it's used as takes it as 'mut'", .{identifier})
        else
            try allocPrint(self.allocator, "Lambda param '{s}' can't be 'mut': the function type it's used as doesn't take it as 'mut'", .{identifier});
        self.log(text, loc);
        return error.LambdaParamMutDiffers;
    }

    pub fn tooManyCaptures(self: *Self, max: usize, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "A lambda can use at most {d} variables from the functions around it", .{max}), loc);
        return error.TooManyCaptures;
    }

    pub fn methodAsValue(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Method '{s}' can't be used as a value yet: wrap the call in a lambda instead, like 'func() -> return x.{s}()'", .{ identifier, identifier }), loc);
        return error.MethodAsValue;
    }

    pub fn builtinAsValue(self: *Self, identifier: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Builtin '{s}' can't be used as a value: call it from a function and use that function instead", .{identifier}), loc);
        return error.BuiltinAsValue;
    }

    pub fn namedArgumentsOnValue(self: *Self, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "A call through a function value can't use named arguments: pass them in order", .{}), loc);
        return error.NamedArgumentsOnValue;
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

    pub fn slotCantFail(self: *Self, slot: []const u8, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' can't be '{s}': a slot can't hold a type that can fail. Handle the errors first, or take them through an error slot ('E: error' with 'E!{s}')", .{ slot, type_name, slot }), loc);
        return error.SlotCantFail;
    }

    pub fn slotNotMarkedError(self: *Self, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' comes before '!', so it must be an error slot: write it as '{s}: error' in the brackets", .{ slot, slot }), loc);
        return error.SlotNotMarkedError;
    }

    pub fn slotNeedsErrorSet(self: *Self, slot: []const u8, type_name: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "'{s}' is an error slot, so it must be an error set, not '{s}'", .{ slot, type_name }), loc);
        return error.SlotNeedsErrorSet;
    }

    pub fn cantMatchSlotErrors(self: *Self, slot: []const u8, loc: Loc) CompileError {
        self.log(try allocPrint(self.allocator, "Can't match the cases of '{s}': its errors depend on the call. Return it, pass it on or store it instead", .{slot}), loc);
        return error.CantMatchSlotErrors;
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

    pub fn tooManyRegisters(self: *Self, function: []const u8, loc: ?Loc) error{ OutOfMemory, TooManyRegisters } {
        self.logAt(
            try allocPrint(
                self.allocator,
                "function '{s}' uses more than 255 registers at once (variables and temporaries); split it into smaller functions",
                .{function},
            ),
            loc,
        );
        return error.TooManyRegisters;
    }

    pub fn functionTooLong(self: *Self, function: []const u8, loc: ?Loc) error{ OutOfMemory, FunctionTooLong } {
        self.logAt(
            try allocPrint(
                self.allocator,
                "function '{s}' is too long (more than 65535 bytecode instructions); split it into smaller functions",
                .{function},
            ),
            loc,
        );
        return error.FunctionTooLong;
    }

    pub fn tooManyStrings(self: *Self) error{TooManyStrings} {
        self.logAt("the program has more than 65536 different string literals", null);
        return error.TooManyStrings;
    }

    fn logAt(self: *Self, message: []const u8, loc: ?Loc) void {
        if (loc) |known| return self.log(message, known);
        if (@import("builtin").is_test) return;
        std.debug.print("error: {s}\n", .{self.withCopyContext(message)});
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

        const file_suffix = if (self.file_path) |path| allocPrint(self.allocator, " in {s}", .{path}) catch "" else "";
        std.log.err(
            "{s} at line {d} col {d}{s}\n--> {d}: {s}\n {s}",
            .{
                msg,
                exactLoc.line.count,
                exactLoc.col,
                file_suffix,
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
    TaskNeedsSpawn,
    UnawaitedTaskCanFail,
    UnhandledError,
    TryOutsideFailingFunction,
    NotAFailingCall,
    ErrorNotInSet,
    PayloadSharesErrors,
    TopLevelLetNotConstant,
    StatementOutsideMain,
    MissingMain,
    DeclarationInsideFunction,
    ExternOutsideStd,
    ExternWithoutBinding,
    SlotParamCantHaveDefault,
    SlotOperatorNotAllowed,
    SlotUsedAsValue,
    NotVisible,
    CantBuildOutsideModule,
    PrivateTypeInPublicSignature,
    LibrariesNotSupported,
    UnknownStdModule,
    ModuleNotFound,
    CantReadModule,
    ImportCycle,
    ModuleNotImported,
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
    BuiltinAsValue,
    MethodAsValue,
    LambdaParamCantHaveDefault,
    TooManyCaptures,
    LambdaParamNeedsType,
    LambdaReturnsDiffer,
    SlotCantFail,
    SlotNotMarkedError,
    SlotNeedsErrorSet,
    CantMatchSlotErrors,
    WrongLambdaParamCount,
    LambdaParamMutDiffers,
    LambdaCallsItself,
    FieldAndMethodSameName,
    NamedArgumentsOnValue,
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

const std = @import("std");
const allocPrint = std.fmt.allocPrint;
const Token = @import("lexer.zig").Token;
