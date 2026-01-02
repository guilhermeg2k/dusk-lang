pub const QjsRuntime = struct {
    const Self = @This();

    pub var stdout_writer: std.io.Writer = undefined;

    rt: *c.JSRuntime,
    ctx: *c.JSContext,

    pub fn init(writer: std.io.Writer) !Self {
        stdout_writer = writer;

        const rt = c.JS_NewRuntime() orelse return error.RuntimeInitFailed;
        const ctx = c.JS_NewContext(rt) orelse {
            c.JS_FreeRuntime(rt);
            return error.ContextInitFailed;
        };

        const global_obj = c.JS_GetGlobalObject(ctx);
        const print_func = c.JS_NewCFunction(ctx, jsEchoFn, "echo", 1);
        _ = c.JS_SetPropertyStr(ctx, global_obj, "echo", print_func);
        c.JS_FreeValue(ctx, global_obj);

        return QjsRuntime{ .rt = rt, .ctx = ctx };
    }

    pub fn deinit(self: *QjsRuntime) void {
        c.JS_FreeContext(self.ctx);
        c.JS_FreeRuntime(self.rt);
    }

    pub fn eval(self: *QjsRuntime, code: []const u8) !void {
        const val = c.JS_Eval(self.ctx, code.ptr, code.len, "main.js", 0);
        defer c.JS_FreeValue(self.ctx, val);

        if (c.JS_IsException(val) != 0) {
            const ex = c.JS_GetException(self.ctx);
            defer c.JS_FreeValue(self.ctx, ex);

            const str = c.JS_ToCString(self.ctx, ex);
            std.log.err("Runtime: {s}\n", .{str});
            defer c.JS_FreeCString(self.ctx, str);

            return error.ExecutionFailed;
        }
    }

    fn jsEchoFn(ctx: ?*c.JSContext, _: c.JSValue, argc: c_int, argv: [*c]c.JSValue) callconv(.c) c.JSValue {
        var i: i32 = 0;
        while (i < argc) : (i += 1) {
            if (i > 0) {
                QjsRuntime.stdout_writer.writeAll(" ") catch {};
            }

            const str_ptr = c.JS_ToCString(ctx, argv[@intCast(i)]);
            const str_len = std.mem.len(str_ptr);
            QjsRuntime.stdout_writer.writeAll(str_ptr[0..str_len]) catch {};

            c.JS_FreeCString(ctx, str_ptr);
        }

        stdout_writer.writeAll("\n") catch {};
        stdout_writer.flush() catch {};

        return c.JSValue{
            .u = .{ .int32 = 0 },
            .tag = c.JS_TAG_UNDEFINED,
        };
    }
};

pub const c = @cImport({
    @cInclude("quickjs.h");
});

const std = @import("std");
