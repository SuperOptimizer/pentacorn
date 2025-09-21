
#include "rv.h"


#define _opcode (opcode & 0x7f)
#define _rd ((opcode >> 7) & 0x1f)
#define _rs1 ((opcode >> 15) & 0x1f)
#define _rs2 ((opcode >> 20) & 0x1f)
#define _shamt _rs2
#define _funct3 ((opcode >> 12) & 0x7)
#define _funct7 ((opcode >> 25) & 0x7f)

rv_instr_e rv_decode(u32 opcode) {
    assert((opcode & 0x3) == 0x3);


    if (_opcode == 0b0110111){ return LUI;}
    if (_opcode == 0b0010111){ return AUIPC;}
    if (_opcode == 0b1101111){ return JAL;}
    if (_opcode == 0b1100111){ return JALR;}
    if (_opcode == 0b1100011) {
        if (_funct3 == 0b000){return BEQ;}
        if (_funct3 == 0b001){return BNE;}
        if (_funct3 == 0b100){return BLT;}
        if (_funct3 == 0b101){return BGE;}
        if (_funct3 == 0b110){return BLTU;}
        if (_funct3 == 0b111){return BGEU;}
    }
    if (_opcode == 0b0000011) {
        if (_funct3 == 0b000){return LB;}
        if (_funct3 == 0b001){return LH;}
        if (_funct3 == 0b010){return LW;}
        if (_funct3 == 0b100){return LBU;}
        if (_funct3 == 0b101){return LHU;}
    }
    if (_opcode == 0b0100011) {
        if (_funct3 == 0b000){return SB;}
        if (_funct3 == 0b001){return SH;}
        if (_funct3 == 0b010){return SW;}
    }
    if (_opcode == 0b0010011) {
        if (_funct3 == 0b000){return ADDI;}
        if (_funct3 == 0b010){return SLTI;}
        if (_funct3 == 0b011){return SLTIU;}
        if (_funct3 == 0b100){return XORI;}
        if (_funct3 == 0b110){return ORI;}
        if (_funct3 == 0b111){return ANDI;}

        if (_funct3 == 0b001){return SLLI;}
        if (_funct3 == 0b101) {
            if (_funct7 == 0b0000000){return SRLI;}
            if (_funct7 == 0b0100000){return SRAI;}
        }
    }
    if (_opcode == 0b0110011) {
        if (_funct7 == 0b0000001) {
            if (_funct3 == 0b000) {return MUL;}
            if (_funct3 == 0b001) {return MULH;}
            if (_funct3 == 0b010) {return MULHSU;}
            if (_funct3 == 0b011) {return MULHU;}
            if (_funct3 == 0b100) {return DIV;}
            if (_funct3 == 0b101) {return DIVU;}
            if (_funct3 == 0b110) {return REM;}
            if (_funct3 == 0b111) {return REMU;}
        }
        if (_funct3 == 0b000) {
            if (_funct7 == 0b0000000){ return ADD;}
            if (_funct7 == 0b0100000){ return SUB;}
        }
        if (_funct3 == 0b001) {return SLL;}
        if (_funct3 == 0b010) {return SLT;}
        if (_funct3 == 0b011) {return SLTU;}
        if (_funct3 == 0b100) {return XOR;}
        if (_funct3 == 0b101) {
            if (_funct7 == 0b0000000){ return SRL;}
            if (_funct7 == 0b0100000){ return SRA;}
        }
        if (_funct3 == 0b110) {return OR;}
        if (_funct3 == 0b111) {return AND;}
    }
    if (_opcode == 0b0001111) {
        //fence, fence.tso, pause, fence.i
    }
    if (_opcode == 0b1110011) {
        if (_funct3 == 0b000) {
            if (_shamt == 1){return ECALL;}
            if (_shamt == 0){return EBREAK;}
        }
        if (_funct3 == 0b001) {return CSRRW;}
        if (_funct3 == 0b010) {return CSRRS;}
        if (_funct3 == 0b011) {return CSRRC;}
        if (_funct3 == 0b101) {return CSRRWI;}
        if (_funct3 == 0b110) {return CSRRSI;}
        if (_funct3 == 0b111) {return CSRRCI;}
    }
    if (_opcode == 0b0101111) {
        if (_funct3 == 0b010) {
            u32 funct5 = _funct7 >> 2;
            if (funct5 == 0b00010) {return LR_W;}
            if (funct5 == 0b00011) {return SC_W;}
            if (funct5 == 0b00001) {return AMOSWAP_W;}
            if (funct5 == 0b00000) {return AMOADD_W;}
            if (funct5 == 0b00100) {return AMOXOR_W;}
            if (funct5 == 0b01100) {return AMOAND_W;}
            if (funct5 == 0b01000) {return AMOOR_W;}
            if (funct5 == 0b10000) {return AMOMIN_W;}
            if (funct5 == 0b10100) {return AMOMAX_W;}
            if (funct5 == 0b11000) {return AMOMINU_W;}
            if (funct5 == 0b11100) {return AMOMAXU_W;}
        }
    }
    return INVALID;
}

void rv_exec(rv* rv, rv_instr_e instr, u32 opcode) {
    switch (instr) {
        case LUI: rv->x[_rd] = opcode & 0xfffff000; break;
        case AUIPC: rv->x[_rd] = rv->pc + (s32)(opcode & 0xfffff000); break;
    }
}