#pragma once

#include <stdint.h>
#include <assert.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
typedef float f32;
typedef double f64;

typedef struct rv {
    u32 x[32];
    u32 pc;
} rv;


typedef enum rv_instr_e {
    LUI,
    AUIPC,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    FENCE,
    FENCE_I,
    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,
    ECALL,
    EBREAK,
    URET,
    SRET,
    MRET,
    WFI,
    SFENCE_VMA,
    LB,
    LH,
    LW, LBU, LHU, SB, SH, SW, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU, MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU, LR_W, SC_W,
    AMOSWAP_W, AMOADD_W, AMOXOR_W, AMOOR_W, AMOMIN_W, AMOMAX_W, AMOMINU_W, AMOMAXU_W, AMOAND_W, INVALID
} rv_instr_e;

