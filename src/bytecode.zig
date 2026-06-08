const std = @import("std");

const OpCode = enum {
    LOAD_CONST,
    LOAD,
    STORE,

    I_ADD,
    I_SUB,
    I_MULT,
    I_DIV,
    I_EQ,
    I_NEQ,
    I_LT,
    I_LE,
    I_GT,
    I_GE,

    F_ADD,
    F_SUB,
    F_MULT,
    F_DIV,
    F_EQ,
    F_NEQ,
    F_LT,
    F_LE,
    F_GT,
    F_GE,

    B_OR,
    B_AND,
    B_EQ,
    B_NEQ,
    B_NOT,

    CALL,
    RETURN,

    JUMP,
    JUMP_IF_FALSE,
};
