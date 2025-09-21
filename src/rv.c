#include "rv.h"


#define _opcode (opcode & 0x7f)
#define _rd ((opcode >> 7) & 0x1f)
#define _rs1 ((opcode >> 15) & 0x1f)
#define _rs2 ((opcode >> 20) & 0x1f)
#define _shamt _rs2
#define _funct3 ((opcode >> 12) & 0x7)
#define _funct7 ((opcode >> 25) & 0x7f)

static s32 decode_b_imm(u32 opcode) {
    u32 imm = ((opcode & 0x80000000) >> 19) | // imm[12] from bit 31
              ((opcode & 0x00000080) << 4) | // imm[11] from bit 7
              ((opcode & 0x7e000000) >> 20) | // imm[10:5] from bits 30:25
              ((opcode & 0x00000f00) >> 7); // imm[4:1] from bits 11:8

    // Sign extend from 13-bit value (bit 12 is sign bit)
    return (_BitInt(13)) imm;
}


// Complete CSR access function
static inline err rv_csr_access(rv *rv, u32 csr, u32 write, u32 *value) {
    switch (csr) {
        // Supervisor CSRs
        case 0x100: // sstatus (view of mstatus)
            if (write) {
                rv->mstatus = (rv->mstatus & ~0x800DE762) | (*value & 0x800DE762);
            } else {
                *value = rv->mstatus & 0x800DE762;
            }
            break;
        case 0x104: // sie (view of mie)
            if (write) {
                rv->mie = (rv->mie & ~0x222) | (*value & 0x222);
            } else {
                *value = rv->mie & 0x222;
            }
            break;
        case 0x105: // stvec
            if (write) rv->stvec = *value;
            else *value = rv->stvec;
            break;
        case 0x106: // scounteren
            if (write) rv->scounteren = *value;
            else *value = rv->scounteren;
            break;
        case 0x140: // sscratch
            if (write) rv->sscratch = *value;
            else *value = rv->sscratch;
            break;
        case 0x141: // sepc
            if (write) rv->sepc = *value;
            else *value = rv->sepc;
            break;
        case 0x142: // scause
            if (write) rv->scause = *value;
            else *value = rv->scause;
            break;
        case 0x143: // stval
            if (write) rv->stval = *value;
            else *value = rv->stval;
            break;
        case 0x144: // sip (view of mip)
            if (write) {
                rv->mip = (rv->mip & ~0x222) | (*value & 0x222);
            } else {
                *value = rv->mip & 0x222;
            }
            break;
        case 0x180: // satp
            if (write) rv->satp = *value;
            else *value = rv->satp;
            break;

        // Machine CSRs
        case 0x300: // mstatus
            if (write) {
                rv->mstatus = (rv->mstatus & ~0x807FFFEC) | (*value & 0x807FFFEC);
            } else {
                *value = rv->mstatus;
            }
            break;
        case 0x301: // misa (read-only in this implementation)
            if (!write) {
                *value = (1 << 30) | // MXL = 1 (XLEN=32)
                         (1 << 0) | // Extension A
                         (1 << 2) | // Extension C
                         (1 << 12) | // Extension M
                         (1 << 18) | // Extension S
                         (1 << 20); // Extension U
            }
            break;
        case 0x302: // medeleg
            if (write) rv->medeleg = *value;
            else *value = rv->medeleg;
            break;
        case 0x303: // mideleg
            if (write) rv->mideleg = *value;
            else *value = rv->mideleg;
            break;
        case 0x304: // mie
            if (write) {
                rv->mie = (rv->mie & ~0xAAA) | (*value & 0xAAA);
            } else {
                *value = rv->mie;
            }
            break;
        case 0x305: // mtvec
            if (write) rv->mtvec = *value;
            else *value = rv->mtvec;
            break;
        case 0x306: // mcounteren
            if (write) rv->mcounteren = *value;
            else *value = rv->mcounteren;
            break;
        case 0x310: // mstatush
            if (write) {
                rv->mstatush = (rv->mstatush & ~0x30) | (*value & 0x30);
            } else {
                *value = rv->mstatush;
            }
            break;
        case 0x340: // mscratch
            if (write) rv->mscratch = *value;
            else *value = rv->mscratch;
            break;
        case 0x341: // mepc
            if (write) rv->mepc = *value;
            else *value = rv->mepc;
            break;
        case 0x342: // mcause
            if (write) rv->mcause = *value;
            else *value = rv->mcause;
            break;
        case 0x343: // mtval
            if (write) rv->mtval = *value;
            else *value = rv->mtval;
            break;
        case 0x344: // mip
            if (write) {
                rv->mip = (rv->mip & ~0xAAA) | (*value & 0xAAA);
            } else {
                *value = rv->mip;
            }
            break;

        // Machine-level Hardware Thread ID
        case 0xF14: // mhartid (read-only)
            if (!write) *value = rv->mhartid;
            break;

        // Machine Vendor/Architecture/Implementation IDs (read-only)
        case 0xF11: // mvendorid
            if (!write) *value = rv->mvendorid;
            break;
        case 0xF12: // marchid
            if (!write) *value = rv->marchid;
            break;
        case 0xF13: // mimpid
            if (!write) *value = rv->mimpid;
            break;

        // Timers and Counters
        case 0xC00: // cycle (mcycle)
            if (write) rv->cycle = *value;
            else *value = rv->cycle;
            break;
        case 0xC80: // cycleh (mcycleh)
            if (write) rv->cycleh = *value;
            else *value = rv->cycleh;
            break;
        case 0xC01: // time (mtime)
            if (write) rv->mtime = *value;
            else *value = rv->mtime;
            break;
        case 0xC81: // timeh (mtimeh)
            if (write) rv->mtimeh = *value;
            else *value = rv->mtimeh;
            break;
        case 0xC02: // instret (same as cycle in this implementation)
            if (write) rv->cycle = *value;
            else *value = rv->cycle;
            break;
        case 0xC82: // instreth (same as cycleh in this implementation)
            if (write) rv->cycleh = *value;
            else *value = rv->cycleh;
            break;

        default:
            return FAIL; // Unknown CSR
    }
    return OK;
}

err rv_vaddr_to_paddr(rv *rv, u32 vaddr, u32 *paddr) {
    return OK;
}

err rv_bus(rv *rv, u32 vaddr, void *inout, bool read, u32 sz) {
    u32 paddr = 0;
    if (rv_vaddr_to_paddr(rv, vaddr, &paddr)) {
        return FAIL;
    }
    if (paddr >= RV_RAM_BASE && paddr < RV_RAM_BASE + RV_RAM_SIZE) {
        if (read) {
            if (sz == 1) {
                *(u8 *) inout = rv->ram[paddr - RV_RAM_BASE];
                return OK;
            }
            if (sz == 2) {
                *(u16 *) inout = *(u16 *) &rv->ram[paddr - RV_RAM_BASE];
                return OK;
            }
            if (sz == 4) {
                *(u32 *) inout = *(u32 *) &rv->ram[paddr - RV_RAM_BASE];
                return OK;
            }
        } else {
            if (sz == 1) {
                rv->ram[paddr - RV_RAM_BASE] = *(u8 *) inout;
                return OK;
            }
            if (sz == 2) {
                *(u16 *) &rv->ram[paddr - RV_RAM_BASE] = *(u16 *) inout;
                return OK;
            }
            if (sz == 4) {
                *(u32 *) &rv->ram[paddr - RV_RAM_BASE] = *(u32 *) inout;
                return OK;
            }
        }
    }
    return FAIL;
}

rv_instr_e rv_decode(u32 opcode) {
    assert((opcode & 0x3) == 0x3);


    if (_opcode == 0b0110111) { return LUI; }
    if (_opcode == 0b0010111) { return AUIPC; }
    if (_opcode == 0b1101111) { return JAL; }
    if (_opcode == 0b1100111) { return JALR; }
    if (_opcode == 0b1100011) {
        if (_funct3 == 0b000) { return BEQ; }
        if (_funct3 == 0b001) { return BNE; }
        if (_funct3 == 0b100) { return BLT; }
        if (_funct3 == 0b101) { return BGE; }
        if (_funct3 == 0b110) { return BLTU; }
        if (_funct3 == 0b111) { return BGEU; }
    }
    if (_opcode == 0b0000011) {
        if (_funct3 == 0b000) { return LB; }
        if (_funct3 == 0b001) { return LH; }
        if (_funct3 == 0b010) { return LW; }
        if (_funct3 == 0b100) { return LBU; }
        if (_funct3 == 0b101) { return LHU; }
    }
    if (_opcode == 0b0100011) {
        if (_funct3 == 0b000) { return SB; }
        if (_funct3 == 0b001) { return SH; }
        if (_funct3 == 0b010) { return SW; }
    }
    if (_opcode == 0b0010011) {
        if (_funct3 == 0b000) { return ADDI; }
        if (_funct3 == 0b010) { return SLTI; }
        if (_funct3 == 0b011) { return SLTIU; }
        if (_funct3 == 0b100) { return XORI; }
        if (_funct3 == 0b110) { return ORI; }
        if (_funct3 == 0b111) { return ANDI; }

        if (_funct3 == 0b001) { return SLLI; }
        if (_funct3 == 0b101) {
            if (_funct7 == 0b0000000) { return SRLI; }
            if (_funct7 == 0b0100000) { return SRAI; }
        }
    }
    if (_opcode == 0b0110011) {
        if (_funct7 == 0b0000001) {
            if (_funct3 == 0b000) { return MUL; }
            if (_funct3 == 0b001) { return MULH; }
            if (_funct3 == 0b010) { return MULHSU; }
            if (_funct3 == 0b011) { return MULHU; }
            if (_funct3 == 0b100) { return DIV; }
            if (_funct3 == 0b101) { return DIVU; }
            if (_funct3 == 0b110) { return REM; }
            if (_funct3 == 0b111) { return REMU; }
        }
        if (_funct3 == 0b000) {
            if (_funct7 == 0b0000000) { return ADD; }
            if (_funct7 == 0b0100000) { return SUB; }
        }
        if (_funct3 == 0b001) { return SLL; }
        if (_funct3 == 0b010) { return SLT; }
        if (_funct3 == 0b011) { return SLTU; }
        if (_funct3 == 0b100) { return XOR; }
        if (_funct3 == 0b101) {
            if (_funct7 == 0b0000000) { return SRL; }
            if (_funct7 == 0b0100000) { return SRA; }
        }
        if (_funct3 == 0b110) { return OR; }
        if (_funct3 == 0b111) { return AND; }
    }
    if (_opcode == 0b0001111) {
        //fence, fence.tso, pause, fence.i
    }
    if (_opcode == 0b1110011) {
        if (_funct3 == 0b000) {
            if (_shamt == 1) { return ECALL; }
            if (_shamt == 0) { return EBREAK; }
        }
        if (_funct3 == 0b001) { return CSRRW; }
        if (_funct3 == 0b010) { return CSRRS; }
        if (_funct3 == 0b011) { return CSRRC; }
        if (_funct3 == 0b101) { return CSRRWI; }
        if (_funct3 == 0b110) { return CSRRSI; }
        if (_funct3 == 0b111) { return CSRRCI; }
    }
    if (_opcode == 0b0101111) {
        if (_funct3 == 0b010) {
            u32 funct5 = _funct7 >> 2;
            if (funct5 == 0b00010) { return LR_W; }
            if (funct5 == 0b00011) { return SC_W; }
            if (funct5 == 0b00001) { return AMOSWAP_W; }
            if (funct5 == 0b00000) { return AMOADD_W; }
            if (funct5 == 0b00100) { return AMOXOR_W; }
            if (funct5 == 0b01100) { return AMOAND_W; }
            if (funct5 == 0b01000) { return AMOOR_W; }
            if (funct5 == 0b10000) { return AMOMIN_W; }
            if (funct5 == 0b10100) { return AMOMAX_W; }
            if (funct5 == 0b11000) { return AMOMINU_W; }
            if (funct5 == 0b11100) { return AMOMAXU_W; }
        }
    }
    return INVALID;
}


void rv_exec(rv *rv, rv_instr_e instr, u32 opcode) {
    switch (instr) {
        case LUI: rv->x[_rd] = opcode & 0xfffff000;
            break;
        case AUIPC: rv->x[_rd] = rv->pc + (s32) (opcode & 0xfffff000);
            break;
        case ADDI: rv->x[_rd] = rv->x[_rs1] + (_BitInt(12)) (opcode >> 20);
            break;
        case SLTI: rv->x[_rd] = rv->x[_rs1] < (_BitInt(12)) (opcode >> 20) ? 1 : 0;
            break;
        case SLTIU: rv->x[_rd] = rv->x[_rs1] < (opcode >> 20) ? 1 : 0;
            break;
        case XORI: rv->x[_rd] = rv->x[_rs1] ^ (_BitInt(12)) (opcode >> 20);
            break;
        case ORI: rv->x[_rd] = rv->x[_rs1] | (_BitInt(12)) (opcode >> 20);
            break;
        case ANDI: rv->x[_rd] = rv->x[_rs1] & (_BitInt(12)) (opcode >> 20);
            break;
        case SLLI: rv->x[_rd] = rv->x[_rs1] << _shamt;
            break;
        case SRLI: rv->x[_rd] = rv->x[_rs1] >> _shamt;
            break;
        case SRAI: rv->x[_rd] = (s32) rv->x[_rs1] >> _shamt;
            break;
        case ADD: rv->x[_rd] = rv->x[_rs1] + rv->x[_rs2];
            break;
        case SUB: rv->x[_rd] = rv->x[_rs1] - rv->x[_rs2];
            break;
        case SLL: rv->x[_rd] = rv->x[_rs1] << (rv->x[_rs2] & 0x1f);
            break;
        case SLT: rv->x[_rd] = (s32) rv->x[_rs1] < (s32) rv->x[_rs2] ? 1 : 0;
            break;
        case SLTU: rv->x[_rd] = rv->x[_rs1] < rv->x[_rs2] ? 1 : 0;
            break;
        case XOR: rv->x[_rd] = rv->x[_rs1] ^ rv->x[_rs2];
            break;
        case SRL: rv->x[_rd] = rv->x[_rs1] >> (rv->x[_rs2] & 0x1f);
            break;
        case SRA: rv->x[_rd] = (s32) rv->x[_rs1] >> (rv->x[_rs2] & 0x1f);
            break;
        case OR: rv->x[_rd] = rv->x[_rs1] | rv->x[_rs2];
            break;
        case AND: rv->x[_rd] = rv->x[_rs1] & rv->x[_rs2];
            break;
        case LB: {
            s8 temp = 0;
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, true, 1);
            rv->x[_rd] = (s32) temp;
            break;
        }
        case LH: {
            s16 temp = 0;
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, true, 2);
            rv->x[_rd] = (s32) temp;
            break;
        }
        case LW: {
            s32 temp = 0;
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, true, 4);
            rv->x[_rd] = temp;
            break;
        }
        case LBU: {
            u8 temp = 0;
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, true, 1);
            rv->x[_rd] = temp;
            break;
        }
        case LHU: {
            u16 temp = 0;
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, true, 2);
            rv->x[_rd] = temp;
            break;
        }
        case SB: {
            u8 temp = rv->x[_rs2];
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, false, 1);
            break;
        }
        case SH: {
            u16 temp = rv->x[_rs2];
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, false, 2);
            break;
        }
        case SW: {
            u32 temp = rv->x[_rd];
            rv_bus(rv, rv->x[_rs1] + (_BitInt(12)) opcode >> 20, &temp, false, 4);
            break;
        }
        case JAL: {
            s32 imm = ((opcode & 0x80000000) >> 11) | // imm[20] from bit 31
                      ((opcode & 0x000ff000)) | // imm[19:12] from bits 19:12
                      ((opcode & 0x00100000) >> 9) | // imm[11] from bit 20
                      ((opcode & 0x7fe00000) >> 20); // imm[10:1] from bits 30:21

            // Sign extend from bit 20
            if (imm & 0x100000) {
                imm |= 0xffe00000;
            }

            rv->x[_rd] = rv->pc + 4;
            rv->pc += imm;
            break;
        }
        case JALR: {
            u32 temp = rv->pc;
            rv->pc = (rv->x[_rs1] + (_BitInt(12)) opcode >> 20) & 0xfffffffe;
            rv->x[_rd] = temp;
        }
        case BEQ: {
            if (rv->x[_rs1] == rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }

        case BNE: {
            if (rv->x[_rs1] != rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }

        case BLT: {
            if ((s32) rv->x[_rs1] < (s32) rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }

        case BGE: {
            if ((s32) rv->x[_rs1] >= (s32) rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }

        case BLTU: {
            if (rv->x[_rs1] < rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }

        case BGEU: {
            if (rv->x[_rs1] >= rv->x[_rs2]) {
                rv->pc += decode_b_imm(opcode);
            }
            break;
        }
        case MUL: {
            rv->x[_rd] = rv->x[_rs1] * rv->x[_rs2];
            break;
        }

        case MULH: {
            s64 result = (s64) (s32) rv->x[_rs1] * (s64) (s32) rv->x[_rs2];
            rv->x[_rd] = result >> 32;
            break;
        }

        case MULHSU: {
            s64 result = (s64) (s32) rv->x[_rs1] * (u64) rv->x[_rs2];
            rv->x[_rd] = result >> 32;
            break;
        }

        case MULHU: {
            u64 result = (u64) rv->x[_rs1] * (u64) rv->x[_rs2];
            rv->x[_rd] = result >> 32;
            break;
        }

        case DIV: {
            s32 dividend = rv->x[_rs1];
            s32 divisor = rv->x[_rs2];

            if (divisor == 0) {
                rv->x[_rd] = -1; // Division by zero
            } else if (dividend == 0x80000000 && divisor == -1) {
                rv->x[_rd] = 0x80000000; // Overflow case
            } else {
                rv->x[_rd] = dividend / divisor;
            }
            break;
        }

        case DIVU: {
            u32 dividend = rv->x[_rs1];
            u32 divisor = rv->x[_rs2];

            if (divisor == 0) {
                rv->x[_rd] = -1; // Division by zero
            } else {
                rv->x[_rd] = dividend / divisor;
            }
            break;
        }

        case REM: {
            s32 dividend = rv->x[_rs1];
            s32 divisor = rv->x[_rs2];

            if (divisor == 0) {
                rv->x[_rd] = dividend; // Remainder by zero returns dividend
            } else if (dividend == 0x80000000 && divisor == -1) {
                rv->x[_rd] = 0; // Overflow case
            } else {
                rv->x[_rd] = dividend % divisor;
            }
            break;
        }

        case REMU: {
            u32 dividend = rv->x[_rs1];
            u32 divisor = rv->x[_rs2];

            if (divisor == 0) {
                rv->x[_rd] = dividend; // Remainder by zero returns dividend
            } else {
                rv->x[_rd] = dividend % divisor;
            }
            break;
        }
        case LR_W: {
            u32 addr = rv->x[_rs1];
            u32 temp;
            if (rv_bus(rv, addr, &temp, true, 4) == OK) {
                rv->x[_rd] = (s32) temp; // Sign-extend
                // In a full implementation, we'd set a reservation here
                // rv->reservation_addr = addr;
                // rv->reservation_valid = true;
            }
            break;
        }

        case SC_W: {
            u32 addr = rv->x[_rs1];
            u32 value = rv->x[_rs2];

            // In a full implementation, we'd check if reservation is still valid
            // For now, we'll always succeed (return 0 for success)
            if (rv_bus(rv, addr, &value, false, 4) == OK) {
                rv->x[_rd] = 0; // Success
            } else {
                rv->x[_rd] = 1; // Failure
            }
            break;
        }

        case AMOSWAP_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;
            u32 new_value = rv->x[_rs2];

            // Load old value
            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value; // Sign-extend
                // Store new value
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOADD_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value; // Sign-extend
                u32 new_value = old_value + rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOXOR_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                u32 new_value = old_value ^ rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOAND_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                u32 new_value = old_value & rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOOR_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                u32 new_value = old_value | rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOMIN_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                s32 signed_old = (s32) old_value;
                s32 signed_new = (s32) rv->x[_rs2];
                u32 new_value = (signed_old < signed_new) ? old_value : rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOMAX_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                s32 signed_old = (s32) old_value;
                s32 signed_new = (s32) rv->x[_rs2];
                u32 new_value = (signed_old > signed_new) ? old_value : rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOMINU_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                u32 new_value = (old_value < rv->x[_rs2]) ? old_value : rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }

        case AMOMAXU_W: {
            u32 addr = rv->x[_rs1];
            u32 old_value;

            if (rv_bus(rv, addr, &old_value, true, 4) == OK) {
                rv->x[_rd] = (s32) old_value;
                u32 new_value = (old_value > rv->x[_rs2]) ? old_value : rv->x[_rs2];
                rv_bus(rv, addr, &new_value, false, 4);
            }
            break;
        }
        case CSRRW: {
            u32 csr = opcode >> 20; // CSR number is in bits 31:20
            u32 old_value = 0;

            // If rd != x0, read the CSR
            if (_rd != 0) {
                if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                    // Handle illegal CSR access
                    break;
                }
            }

            // Write rs1 value to CSR
            u32 new_value = rv->x[_rs1];
            if (rv_csr_access(rv, csr, 1, &new_value) != OK) {
                // Handle illegal CSR access
                break;
            }

            // Write old CSR value to rd (if rd != x0)
            if (_rd != 0) {
                rv->x[_rd] = old_value;
            }
            break;
        }

        case CSRRS: {
            u32 csr = opcode >> 20;
            u32 old_value = 0;

            // Read CSR
            if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                break;
            }

            // Write to rd
            rv->x[_rd] = old_value;

            // If rs1 != x0, set bits in CSR
            if (_rs1 != 0) {
                u32 new_value = old_value | rv->x[_rs1];
                rv_csr_access(rv, csr, 1, &new_value);
            }
            break;
        }

        case CSRRC: {
            u32 csr = opcode >> 20;
            u32 old_value = 0;

            // Read CSR
            if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                break;
            }

            // Write to rd
            rv->x[_rd] = old_value;

            // If rs1 != x0, clear bits in CSR
            if (_rs1 != 0) {
                u32 new_value = old_value & ~rv->x[_rs1];
                rv_csr_access(rv, csr, 1, &new_value);
            }
            break;
        }

        case CSRRWI: {
            u32 csr = opcode >> 20;
            u32 uimm = _rs1; // 5-bit immediate is in the rs1 field
            u32 old_value = 0;

            // If rd != x0, read the CSR
            if (_rd != 0) {
                if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                    break;
                }
                rv->x[_rd] = old_value;
            }

            // Write immediate value to CSR
            u32 new_value = uimm;
            rv_csr_access(rv, csr, 1, &new_value);
            break;
        }

        case CSRRSI: {
            u32 csr = opcode >> 20;
            u32 uimm = _rs1; // 5-bit immediate
            u32 old_value = 0;

            // Read CSR
            if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                break;
            }

            // Write to rd
            rv->x[_rd] = old_value;

            // If uimm != 0, set bits in CSR
            if (uimm != 0) {
                u32 new_value = old_value | uimm;
                rv_csr_access(rv, csr, 1, &new_value);
            }
            break;
        }

        case CSRRCI: {
            u32 csr = opcode >> 20;
            u32 uimm = _rs1; // 5-bit immediate
            u32 old_value = 0;

            // Read CSR
            if (rv_csr_access(rv, csr, 0, &old_value) != OK) {
                break;
            }

            // Write to rd
            rv->x[_rd] = old_value;

            // If uimm != 0, clear bits in CSR
            if (uimm != 0) {
                u32 new_value = old_value & ~uimm;
                rv_csr_access(rv, csr, 1, &new_value);
            }
            break;
        }
    }
}
