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

typedef enum err {
   OK=0,
    FAIL=1
} err;

typedef struct rv {
    u32 x[32];
    u32 pc;
    u8* ram;

    u32 reservation_addr;
    bool reservation_valid;

    u32 /* sstatus, */ sie, stvec, scounteren, sscratch, sepc, scause, stval, sip, satp;
    u32 mstatus, misa, medeleg, mideleg, mie, mtvec, mcounteren, mstatush,
        mscratch, mepc, mcause, mtval, mip, mtime, mtimeh, mvendorid, marchid,
        mimpid, mhartid;
    u32 cycle, cycleh;
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


#define RV_RAM_BASE 0x80000000UL
#define RV_RAM_SIZE (1024UL * 1024UL * 128UL)
#define RV_DTB_OFFSET 0x2000000UL

#define RV_PLIC0_BASE 0xC000000UL
#define RV_CLINT0_BASE 0x2000000UL
#define RV_UART0_BASE 0x3000000UL
