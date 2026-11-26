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
pub const prepare_persistent: c_uint = 0x01;
pub const transient_destructor: isize = -1;
pub const utf8_encoding: u8 = 1;
pub const integer_type: c_int = 1;
pub const float_type: c_int = 2;
pub const text_type: c_int = 3;
pub const blob_type: c_int = 4;

pub extern fn sqlite3_libversion() [*:0]const u8;
pub extern fn sqlite3_open_v2(filename: [*:0]const u8, database: *?*Database, flags: c_int, vfs: ?[*:0]const u8) c_int;
pub extern fn sqlite3_close_v2(database: ?*Database) c_int;
pub extern fn sqlite3_exec(database: *Database, sql: [*:0]const u8, callback: ?*const anyopaque, argument: ?*anyopaque, message: ?*?[*:0]u8) c_int;
pub extern fn sqlite3_prepare_v2(database: *Database, sql: [*]const u8, byte_count: c_int, statement: *?*Statement, tail: *?[*]const u8) c_int;
pub extern fn sqlite3_prepare_v3(database: *Database, sql: [*]const u8, byte_count: c_int, flags: c_uint, statement: *?*Statement, tail: *?[*]const u8) c_int;
pub extern fn sqlite3_step(statement: *Statement) c_int;
pub extern fn sqlite3_reset(statement: *Statement) c_int;
pub extern fn sqlite3_clear_bindings(statement: *Statement) c_int;
pub extern fn sqlite3_bind_parameter_count(statement: *Statement) c_int;
pub extern fn sqlite3_bind_parameter_index(statement: *Statement, name: [*:0]const u8) c_int;
pub extern fn sqlite3_bind_parameter_name(statement: *Statement, index: c_int) ?[*:0]const u8;
pub extern fn sqlite3_bind_null(statement: *Statement, index: c_int) c_int;
pub extern fn sqlite3_bind_int64(statement: *Statement, index: c_int, value: i64) c_int;
pub extern fn sqlite3_bind_double(statement: *Statement, index: c_int, value: f64) c_int;
pub extern fn sqlite3_bind_text64(statement: *Statement, index: c_int, text: [*]const u8, byte_count: u64, destructor: isize, encoding: u8) c_int;
pub extern fn sqlite3_changes64(database: *Database) i64;
pub extern fn sqlite3_column_count(statement: *Statement) c_int;
pub extern fn sqlite3_column_name(statement: *Statement, column: c_int) ?[*:0]const u8;
pub extern fn sqlite3_column_type(statement: *Statement, column: c_int) c_int;
pub extern fn sqlite3_column_int64(statement: *Statement, column: c_int) i64;
pub extern fn sqlite3_column_double(statement: *Statement, column: c_int) f64;
pub extern fn sqlite3_column_text(statement: *Statement, column: c_int) ?[*]const u8;
pub extern fn sqlite3_column_blob(statement: *Statement, column: c_int) ?*const anyopaque;
pub extern fn sqlite3_column_bytes(statement: *Statement, column: c_int) c_int;
pub extern fn sqlite3_finalize(statement: ?*Statement) c_int;
pub extern fn sqlite3_busy_timeout(database: *Database, milliseconds: c_int) c_int;
pub extern fn sqlite3_interrupt(database: *Database) void;
pub extern fn sqlite3_errmsg(database: ?*Database) [*:0]const u8;
pub extern fn sqlite3_error_offset(database: *Database) c_int;
pub extern fn sqlite3_total_changes64(database: *Database) i64;
pub extern fn sqlite3_last_insert_rowid(database: *Database) i64;
pub extern fn sqlite3_get_autocommit(database: *Database) c_int;
