pub const Database = opaque {};
pub const Statement = opaque {};

pub const ResultCode = enum(c_int) {
    ok = 0,
    sql_error = 1,
    permission = 3,
    busy = 5,
    locked = 6,
    read_only = 8,
    interrupted = 9,
    corrupt = 11,
    full = 13,
    cant_open = 14,
    constraint = 19,
    row = 100,
    done = 101,
    not_a_database = 26,
    _,

    pub fn of(code: c_int) ResultCode {
        return @enumFromInt(code & 0xff);
    }
};

pub const open_read_only: c_int = 0x00000001;
pub const open_read_write: c_int = 0x00000002;
pub const open_create: c_int = 0x00000004;
pub const open_extended_result_codes: c_int = 0x02000000;

pub extern fn sqlite3_libversion() [*:0]const u8;
pub extern fn sqlite3_open_v2(filename: [*:0]const u8, database: *?*Database, flags: c_int, vfs: ?[*:0]const u8) c_int;
pub extern fn sqlite3_close_v2(database: ?*Database) c_int;
pub extern fn sqlite3_exec(database: *Database, sql: [*:0]const u8, callback: ?*const anyopaque, argument: ?*anyopaque, message: ?*?[*:0]u8) c_int;
pub extern fn sqlite3_prepare_v2(database: *Database, sql: [*]const u8, byte_count: c_int, statement: *?*Statement, tail: *?[*]const u8) c_int;
pub extern fn sqlite3_step(statement: *Statement) c_int;
pub extern fn sqlite3_finalize(statement: ?*Statement) c_int;
pub extern fn sqlite3_busy_timeout(database: *Database, milliseconds: c_int) c_int;
pub extern fn sqlite3_errmsg(database: ?*Database) [*:0]const u8;
pub extern fn sqlite3_error_offset(database: *Database) c_int;
pub extern fn sqlite3_total_changes64(database: *Database) i64;
pub extern fn sqlite3_last_insert_rowid(database: *Database) i64;
