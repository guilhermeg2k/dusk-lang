pub const QJSRunTime = struct {
    rt: *qjs.JSRuntime,
    ctx: *qjs.JSContext,

    pub fn init() !QJSRunTime {
        const rt = qjs.JS_NewRuntime() orelse return error.RuntimeInitFailed;

        const ctx = qjs.JS_NewContext(rt) orelse {
            qjs.JS_FreeRuntime(rt);
            return error.ContextInitFailed;
        };

        return QJSRunTime{
            .rt = rt,
            .ctx = ctx,
        };
    }

    pub fn deinit(self: *QJSRunTime) void {
        qjs.JS_FreeContext(self.ctx);
        qjs.JS_FreeRuntime(self.rt);
    }

    pub fn eval(self: *QJSRunTime, code: []const u8) !void {
        const val = qjs.JS_Eval(self.ctx, code.ptr, code.len, "input.js", 0);
        defer qjs.JS_FreeValue(self.ctx, val);

        if (qjs.JS_IsException(val) != 0) {
            const ex_val = qjs.JS_GetException(self.ctx);
            defer qjs.JS_FreeValue(self.ctx, ex_val);
            // 1. Get the properties from the Exception Object
            //
            const file_val = qjs.JS_GetPropertyStr(self.ctx, ex_val, "fileName");
            const line_val = qjs.JS_GetPropertyStr(self.ctx, ex_val, "lineNumber");
            const stack_val = qjs.JS_GetPropertyStr(self.ctx, ex_val, "stack");

            // Clean up these JSValues later
            defer {
                qjs.JS_FreeValue(self.ctx, file_val);
                qjs.JS_FreeValue(self.ctx, line_val);
                qjs.JS_FreeValue(self.ctx, stack_val);
            }

            // 2. Convert to Zig Types
            const file_str = qjs.JS_ToCString(self.ctx, file_val);
            const msg_str = qjs.JS_ToCString(self.ctx, ex_val); // The main error message
            var line_int: i32 = -1;
            _ = qjs.JS_ToInt32(self.ctx, &line_int, line_val);

            defer qjs.JS_FreeCString(self.ctx, file_str);
            defer qjs.JS_FreeCString(self.ctx, msg_str);

            std.log.err("\n=== JS EXCEPTION ===\n", .{});
            std.log.err("File: {s}\n", .{std.mem.span(file_str)});
            std.log.err("Line: {d}\n", .{line_int});
            std.log.err("Error: {s}\n", .{std.mem.span(msg_str)});

            // 4. Print Stack Trace (if available)
            if (qjs.JS_IsUndefined(stack_val) == 0) {
                const stack_str = qjs.JS_ToCString(self.ctx, stack_val);
                defer qjs.JS_FreeCString(self.ctx, stack_str);
                std.log.err("Stack:\n{s}\n", .{std.mem.span(stack_str)});
            }
            return error.ExecutionFailed;
        }

        if (qjs.JS_IsUndefined(val) == 0) {
            const str_ptr = qjs.JS_ToCString(self.ctx, val);
            if (str_ptr) |str| {
                defer qjs.JS_FreeCString(self.ctx, str);
                std.log.info("{s}\n", .{std.mem.span(str)});
            }
        }
    }
};

const qjs = @cImport({
    @cInclude("quickjs.h");
});

const std = @import("std");
