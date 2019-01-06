#include <stdio.h>

#include "Cpu.h"
#include "Structure.h"
#include "Utility.h"

#define DEBUG true

struct Operation Operations[256] =
{
  { "BRK", 1, 7, Implied, Break }, { "ORA", 2, 6, IndirectX, IncOr },
  { "INV", 1, 1, Invalid, NULL }, { "SLO", 2, 6, IndirectX, ShiftLeftOr },
  { "NOP", 2, 3, ZeroPage, NoOp }, { "ORA", 2, 3, ZeroPage, IncOr },
  { "ASL", 2, 5, ZeroPage, AShiftLeft }, { "SLO", 2, 1, ZeroPage, ShiftLeftOr },
  { "PHP", 1, 3, Implied, PushState }, { "ORA", 2, 2, Immediate, IncOr },
  { "ASL", 1, 2, Accumulator, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "NOP", 3, 4, Absolute, NoOp }, { "ORA", 3, 4, Absolute, IncOr },
  { "ASL", 3, 6, Absolute, AShiftLeft }, { "SLO", 3, 1, Absolute, ShiftLeftOr },
  // 0x10
  { "BPL", 2, 2, Relative, BranchPos }, { "ORA", 2, 5, IndirectY, IncOr },
  { "INV", 1, 1, Invalid, NULL }, { "SLO", 2, 1, IndirectY, ShiftLeftOr },
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "ORA", 2, 4, ZeroPageX, IncOr },
  { "ASL", 2, 6, ZeroPageX, AShiftLeft }, { "SLO", 2, 1, ZeroPageX, ShiftLeftOr },
  { "CLC", 1, 2, Implied, ClearCarry }, { "ORA", 3, 4, AbsoluteY, IncOr },
  { "NOP", 1, 2, Invalid, NoOp }, { "SLO", 3, 1, AbsoluteY, ShiftLeftOr },
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "ORA", 3, 4, AbsoluteX, IncOr },
  { "ASL", 3, 7, AbsoluteX, AShiftLeft }, { "SLO", 3, 1, AbsoluteX, ShiftLeftOr },
  // 0x20
  { "JSR", 3, 6, Absolute, StartMethod }, { "AND", 2, 6, IndirectX, And },
  { "INV", 1, 1, Invalid, NULL }, { "RLA", 2, 1, IndirectX, RotateLeftAnd },
  { "BIT", 2, 3, ZeroPage, BitTest }, { "AND", 2, 3, ZeroPage, And },
  { "ROL", 2, 5, ZeroPage, RotateLeft }, { "RLA", 2, 1, ZeroPage, RotateLeftAnd },
  { "PLP", 1, 4, Implied, PullState }, { "AND", 2, 2, Immediate, And },
  { "ROL", 1, 2, Accumulator, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "BIT", 3, 4, Absolute, BitTest }, { "AND", 3, 4, Absolute, And },
  { "ROL", 3, 6, Absolute, RotateLeft }, { "RLA", 3, 1, Absolute, RotateLeftAnd },
  // 0x30
  { "BMI", 2, 2, Relative, BranchNeg }, { "AND", 2, 5, IndirectY, And },
  { "INV", 1, 1, Invalid, NULL }, { "RLA", 2, 1, IndirectY, RotateLeftAnd },
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "AND", 2, 4, ZeroPageX, And },
  { "ROL", 2, 6, ZeroPageX, RotateLeft }, { "RLA", 2, 1, ZeroPageX, RotateLeftAnd },
  { "SEC", 1, 2, Implied, SetCarry }, { "AND", 3, 4, AbsoluteY, And },
  { "NOP", 1, 2, Invalid, NoOp }, { "RLA", 3, 1, AbsoluteY, RotateLeftAnd },
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "AND", 3, 4, AbsoluteX, And },
  { "ROL", 3, 7, AbsoluteX, RotateLeft }, { "RLA", 3, 1, AbsoluteX, RotateLeftAnd },
  // 0x40
  { "RTI", 1, 6, Implied, EndInterrupt }, { "EOR", 2, 6, IndirectX, ExOr },
  { "INV", 1, 1, Invalid, NULL }, { "SRE", 2, 1, IndirectX, ShiftRightExOr },
  { "NOP", 2, 3, ZeroPage, NoOp }, { "EOR", 2, 3, ZeroPage, ExOr },
  { "LSR", 2, 5, ZeroPage, LShiftRight }, { "SRE", 2, 1, ZeroPage, ShiftRightExOr },
  { "PHA", 1, 3, Implied, PushAccum }, { "EOR", 2, 2, Immediate, ExOr },
  { "LSR", 1, 2, Accumulator, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  // Officially, the Jump is of type Absolute.
  { "JMP", 3, 3, Immediate, Jump }, { "EOR", 3, 4, Absolute, ExOr },
  { "LSR", 3, 6, Absolute, LShiftRight }, { "SRE", 3, 1, Absolute, ShiftRightExOr },
  // 0x50
  { "BVC", 2, 2, Relative, BranchOverflowClear }, { "EOR", 2, 5, IndirectY, ExOr },
  { "INV", 1, 1, Invalid, NULL }, { "SRE", 2, 1, IndirectY, ShiftRightExOr },
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "EOR", 2, 4, ZeroPageX, ExOr },
  { "LSR", 2, 6, ZeroPageX, LShiftRight }, { "SRE", 2, 1, ZeroPageX, ShiftRightExOr },
  { "CLI", 1, 2, Implied, ClearInterDisable }, { "EOR", 3, 4, AbsoluteY, ExOr },
  { "NOP", 1, 2, Invalid, NoOp }, { "SRE", 3, 1, AbsoluteY, ShiftRightExOr },
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "EOR", 3, 4, AbsoluteX, ExOr },
  { "LSR", 3, 7, AbsoluteX, LShiftRight }, { "SRE", 3, 1, AbsoluteX, ShiftRightExOr },
  // 0x60
  { "RTS", 1, 6, Implied, EndMethod }, { "ADC", 2, 6, IndirectX, AddWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "RRA", 2, 1, IndirectX, RotateRightAdd },
  { "NOP", 2, 3, ZeroPage, NoOp }, { "ADC", 2, 3, ZeroPage, AddWCarry },
  { "ROR", 2, 5, ZeroPage, RotateRight }, { "RRA", 2, 1, ZeroPage, RotateRightAdd },
  { "PLA", 1, 4, Implied, PullAccum }, { "ADC", 2, 2, Immediate, AddWCarry },
  { "ROR", 1, 2, Accumulator, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "JMP", 3, 5, Indirect, Jump }, { "ADC", 3, 4, Absolute, AddWCarry },
  { "ROR", 3, 6, Absolute, RotateRight }, { "RRA", 3, 1, Absolute, RotateRightAdd },
  // 0x70
  { "BVS", 2, 2, Relative, BranchOverflowSet }, { "ADC", 2, 5, IndirectY, AddWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "RRA", 2, 1, IndirectY, RotateRightAdd },
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "ADC", 2, 4, ZeroPageX, AddWCarry },
  { "ROR", 2, 6, ZeroPageX, RotateRight }, { "RRA", 2, 1, ZeroPageX, RotateRightAdd },
  { "SEI", 1, 2, Implied, SetInterDisable }, { "ADC", 3, 4, AbsoluteY, AddWCarry },
  { "NOP", 1, 2, Invalid, NoOp }, { "RRA", 3, 1, AbsoluteY, RotateRightAdd },
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "ADC", 3, 4, AbsoluteX, AddWCarry },
  { "ROR", 3, 7, AbsoluteX, RotateRight }, { "RRA", 3, 1, AbsoluteX, RotateRightAdd },
  // 0x80
  { "NOP", 2, 2, Immediate, NoOp }, { "STA", 2, 6, IndirectX, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "SAX", 2, 6, IndirectX, StoreAccumX },
  { "STY", 2, 3, ZeroPage, StoreY }, { "STA", 2, 3, ZeroPage, StoreAccum },
  { "STX", 2, 3, ZeroPage, StoreX }, { "SAX", 2, 3, ZeroPage, StoreAccumX },
  { "DEY", 1, 2, Implied, DecrementY }, { "INV", 1, 1, Invalid, NULL },
  { "TXA", 1, 2, Implied, XToAccum }, { "INV", 1, 1, Invalid, NULL },
  { "STY", 3, 4, Absolute, StoreY }, { "STA", 3, 4, Absolute, StoreAccum },
  // 0x8F functions as SAX
  { "STX", 3, 4, Absolute, StoreX }, { "SAX", 3, 4, Absolute, StoreAccumX },
  { "BCC", 2, 2, Relative, BranchCarryClear }, { "STA", 2, 6, IndirectY, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "STY", 2, 4, ZeroPageX, StoreY }, { "STA", 2, 4, ZeroPageX, StoreAccum },
  // 0x97 functions as SAX
  { "STX", 2, 4, ZeroPageY, StoreX }, { "SAX", 2, 4, ZeroPageY, StoreAccumX },
  { "TYA", 1, 2, Implied, YToAccum }, { "STA", 3, 5, AbsoluteY, StoreAccum },
  { "TXS", 1, 2, Implied, XToSP }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "STA", 3, 5, AbsoluteX, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 2, 2, Immediate, LoadY }, { "LDA", 2, 6, IndirectX, LoadAccum },
  // 0xA3 treated as LoadX/Accum
  { "LDX", 2, 2, Immediate, LoadX }, { "LAX", 2, 6, IndirectX, LoadAccumX },
  { "LDY", 2, 3, ZeroPage, LoadY }, { "LDA", 2, 3, ZeroPage, LoadAccum },
  // 0xA7 treated as LoadX/Accum
  { "LDX", 2, 3, ZeroPage, LoadX }, { "LAX", 2, 3, ZeroPage, LoadAccumX },
  { "TAY", 1, 2, Implied, AccumToY }, { "LDA", 2, 2, Immediate, LoadAccum },
  { "TAX", 1, 2, Implied, AccumToX }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 3, 4, Absolute, LoadY }, { "LDA", 3, 4, Absolute, LoadAccum },
  // 0xAF treated as Load X/Accum
  { "LDX", 3, 4, Absolute, LoadX }, { "LAX", 3, 4, Absolute, LoadAccumX },
  { "BCS", 2, 2, Relative, BranchCarrySet }, { "LDA", 2, 5, IndirectY, LoadAccum },
  // , 0xB3 treated as Load X/Accum
  { "INV", 1, 1, Invalid, NULL }, { "LAX", 2, 5, IndirectY, LoadAccumX },
  { "LDY", 2, 4, ZeroPageX, LoadY }, { "LDA", 2, 4, ZeroPageX, LoadAccum },
  // 0xB7 treated as LoadX/Accum
  { "LDX", 2, 4, ZeroPageY, LoadX }, { "LAX", 2, 1, ZeroPageY, LoadAccumX },
  { "CLV", 1, 2, Implied, ClearOverflow }, { "LDA", 3, 4, AbsoluteY, LoadAccum },
  { "TSX", 1, 2, Implied, SPToX }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 3, 4, AbsoluteX, LoadY }, { "LDA", 3, 4, AbsoluteX, LoadAccum },
  // 0xBF treated as LoadX/Accum
  { "LDX", 3, 4, AbsoluteY, LoadX }, { "LAX", 3, 4, AbsoluteY, LoadAccumX },
  { "CPY", 2, 2, Immediate, CompareY }, { "CMP", 2, 6, IndirectX, Compare },
  // , 0xC3 decrements and compares
  { "INV", 1, 1, Invalid, NULL }, { "DCP", 2, 8, IndirectX, DecCompare },
  { "CPY", 2, 3, ZeroPage, CompareY }, { "CMP", 2, 3, ZeroPage, Compare },
  // 0xC7 decrements and compares
  { "DEC", 2, 5, ZeroPage, Decrement }, { "DCP", 2, 5, ZeroPage, DecCompare },
  { "INY", 1, 2, Implied, IncrementY }, { "CMP", 2, 2, Immediate, Compare },
  { "DEX", 1, 2, Implied, DecrementX }, { "INV", 1, 1, Invalid, NULL },
  { "CPY", 3, 4, Absolute, CompareY }, { "CMP", 3, 4, Absolute, Compare },
  // 0xCF decrements and compares
  { "DEC", 3, 6, Absolute, Decrement }, { "DCP", 3, 6, Absolute, DecCompare },
  { "BNE", 2, 2, Relative, BranchUnequal }, { "CMP", 2, 5, IndirectY, Compare },
  // , 0xD3 decrements and compares
  { "INV", 1, 1, Invalid, NULL }, { "DCP", 2, 5, IndirectY, DecCompare },
  // 0xD4 treated as 1 arg NOP
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "CMP", 2, 4, ZeroPageX, Compare },
  // 0xD7 decrements and compares
  { "DEC", 2, 6, ZeroPageX, Decrement }, { "DCP", 2, 6, ZeroPageX, DecCompare },
  { "CLD", 1, 2, Implied, ClearDecimal }, { "CMP", 3, 4, AbsoluteY, Compare },
  // 0xDA treated as 0 arg NOP, 0xDB decrements and compares
  { "NOP", 1, 2, Invalid, NoOp }, { "DCP", 3, 9, AbsoluteY, DecCompare },
  // 0xDC treated as 2 arg NOP
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "CMP", 3, 4, AbsoluteX, Compare },
  // 0xDF decrements and compares
  { "DEC", 3, 7, AbsoluteX, Decrement }, { "DCP", 3, 9, AbsoluteX, DecCompare },
  { "CPX", 2, 2, Immediate, CompareX }, { "SBC", 2, 6, IndirectX, SubWCarry },
  // , 0xE3 increments and does subtract with carry
  { "INV", 1, 1, Invalid, NULL }, { "ISB", 2, 1, IndirectX, IncSubWCarry },
  { "CPX", 2, 3, ZeroPage, CompareX }, { "SBC", 2, 3, ZeroPage, SubWCarry },
  // 0xE7 increments and subtracts
  { "INC", 2, 5, ZeroPage, Increment }, { "ISB", 2, 2, ZeroPage, IncSubWCarry },
  { "INX", 1, 2, Implied, IncrementX }, { "SBC", 2, 2, Immediate, SubWCarry },
  // 0xEA is the official NoOp, 0xEB functions as subtract
  { "NOP", 1, 2, Implied, NoOp }, { "SBC", 2, 2, Immediate, SubWCarry },
  { "CPX", 3, 4, Absolute, CompareX }, { "SBC", 3, 4, Absolute, SubWCarry },
  // 0xEF increments and subtracts
  { "INC", 3, 6, Absolute, Increment }, { "ISB", 3, 4, Absolute, IncSubWCarry },
  { "BEQ", 2, 2, Relative, BranchEqual }, { "SBC", 2, 5, IndirectY, SubWCarry },
  // , 0xF3 increments and subtracts
  { "INV", 1, 1, Invalid, NULL }, { "ISB", 2, 1, IndirectY, IncSubWCarry },
  // 0xF4 treated as 1 arg NOP
  { "NOP", 2, 4, ZeroPageX, NoOp }, { "SBC", 2, 4, ZeroPageX, SubWCarry },
  // 0xF7 increments and subtracts
  { "INC", 2, 6, ZeroPageX, Increment }, { "ISB", 2, 1, ZeroPageX, IncSubWCarry },
  { "SED", 1, 2, Implied, SetDecimal }, { "SBC", 3, 4, AbsoluteY, SubWCarry },
  // 0xFA treated as 0 arg NOP, 0xFB increments and subtracts
  { "NOP", 1, 2, Invalid, NoOp }, { "ISB", 3, 1, AbsoluteY, IncSubWCarry },
  // 0xFC treated as 2 arg NOP
  { "NOP", 3, 4, AbsoluteX, NoOp }, { "SBC", 3, 4, AbsoluteX, SubWCarry },
  // 0xFF increments and subtracts
  { "INC", 3, 7, AbsoluteX, Increment }, { "ISB", 3, 1, AbsoluteX, IncSubWCarry }
};

unsigned char PrintInstruction(unsigned char * instruction)
{
  struct Operation opcode = Operations[instruction[0]];
  switch (opcode.Length)
  {
    case 1:
      printf("%s\n", opcode.Name);
      break;
    case 2:
      printf("%s $%X\n", opcode.Name, instruction[1]);
      break;
    case 3:
      printf("%s $%X\n", opcode.Name,
             ConvertFromLittleEndian(instruction[1], instruction[2]));
      break;
    default:
      printf("Error in opcode length table.\n");
  }
  return opcode.Length;
}

bool DebugInstruction(unsigned char * instruction, struct MachineStructure * state,
                      char * expected, bool log, bool compare)
{
  if (log)
  {
    LogInstruction(instruction, state);
  }

  if (compare)
  {
    return CompareInstruction(instruction, state, expected);
  }

  return true;
}

void LogInstruction(unsigned char * instruction, struct MachineStructure * state)
{
  printf("%X ", state->Registers->ProgramCounter);
  struct Operation opcode = Operations[instruction[0]];
  for (unsigned char l = 0; l < opcode.Length; ++l)
  {
    printf("%X ", *(instruction + l));
  }
  printf("\t %s ", opcode.Name);

  switch (opcode.Length)
  {
    case 2:
      printf("$%02X", (char)instruction[1]);
      break;
    case 3:
      printf("$%04X", ConvertFromLittleEndian(instruction[1], instruction[2]));
      break;
    default:
      printf("\t");
  }
  printf("\t\t\t");
  printf("A:%X X:%X Y:%X P:%X SP:%X CYC:%3d\n",
         state->Registers->Accumulator,
         state->Registers->X,
         state->Registers->Y,
         state->Registers->StatusFlags,
         state->Registers->StackPointer,
         state->CycleCount);
}

bool CompareInstruction(unsigned char * instruction, struct MachineStructure * state,
                        char * expected)
{
  bool goodInstruction = true;
  if (!CompareHexStringToInt(state->Registers->ProgramCounter, expected))
  {
    printf("Incorrect program counter %x\n", state->Registers->ProgramCounter);
    return false;
  }
  expected += 4;
  expected = AdvanceToNextWhitespace(expected);

  struct Operation opcode = Operations[instruction[0]];
  for (unsigned char i = 0; i < opcode.Length; ++i)
  {
    if (!CompareHexStringToInt(instruction[i], expected))
    {
      printf("Incorrect code:  %x\n", instruction[i]);
      goodInstruction = false;
    }
    expected += 3;
  }
  expected = AdvanceToNextWhitespace(expected);

  // Skip ahead to status registers
  while (!(*expected == 'A' && *(expected + 1) == ':'))
  {
    ++expected;
  }

  // Accumulator
  expected += 2;
  if (!CompareHexStringToInt(state->Registers->Accumulator, expected))
  {
    printf("Incorrect accumulator value: %x\n", state->Registers->Accumulator);
    goodInstruction = false;
  }

  // X register
  expected += 5;
  if (!CompareHexStringToInt(state->Registers->X, expected))
  {
    printf("Incorrect x register value: %x\n", state->Registers->X);
    goodInstruction = false;
  }

  // Y register
  expected += 5;
  if (!CompareHexStringToInt(state->Registers->Y, expected))
  {
    printf("Incorrect y register value: %x\n", state->Registers->Y);
    goodInstruction = false;
  }

  // Status flags
  expected += 5;
  if (!CompareHexStringToInt(state->Registers->StatusFlags, expected))
  {
    printf("Incorrect status flags: %x\n", state->Registers->StatusFlags);
    goodInstruction = false;
  }

  // Stack pointer
  expected += 6;
  if (!CompareHexStringToInt(state->Registers->StackPointer, expected))
  {
    printf("Invalid stack pointer: %x\n", state->Registers->StackPointer);
    goodInstruction = false;
  }

  // TODO : Cycle count. Numbers in 'official' log look wrong.

  return goodInstruction;
}

bool ProcessInstruction(struct MachineStructure * state, char * expected,
                        bool log, bool compare)
{
  uint16_t PC = state->Registers->ProgramCounter;
  unsigned char * op = ConvertAddress(state, PC);
  bool valid = DebugInstruction(op, state, expected, log, compare);

  struct Operation opcode = Operations[*op];
  state->CycleCount += opcode.Cycles;
  opcode.Func(state, opcode.Mode);
  state->Registers->ProgramCounter += opcode.Length;
  return valid;
}

void SetStatusFlags(struct RegisterStructure * registers, uint8_t value)
{
  SetOrClearFlags(!value, registers, ZERO_FLAG);
  SetOrClearFlags(value & NEGATIVE_FLAG, registers, NEGATIVE_FLAG);
}

void AddWCarry(struct MachineStructure * state, enum AddressMode mode)
{
  int8_t original = state->Registers->Accumulator;
  uint8_t arg = CalculateShortArgument(state, mode);
  int16_t sum = state->Registers->Accumulator;
  sum += arg;
  sum += (state->Registers->StatusFlags & CARRY_FLAG);
  state->Registers->Accumulator = (uint8_t)sum;

  SetStatusFlags(state->Registers, state->Registers->Accumulator);
  // Overflow if the signs of the added numbers are the same
  // but differ from the sign of the result.
  SetOrClearFlags(((original < 0) == (((int8_t)arg) < 0)) &&
                  ((original < 0) != (((int8_t)state->Registers->Accumulator) < 0)),
                  state->Registers, OVERFLOW_FLAG);
  SetOrClearFlags(sum > state->Registers->Accumulator, state->Registers, CARRY_FLAG);
}

uint8_t SubWCarryImpl(struct MachineStructure * state, uint8_t val, uint8_t sub)
{
  uint8_t result;
  int8_t original = val;
  int16_t sum = val;
  sum -= sub;
  sum -= (1 - (state->Registers->StatusFlags & CARRY_FLAG));
  result = (uint8_t)sum;

  SetStatusFlags(state->Registers, result);
  // Overflow if the sign of the subracted number differs from the original
  // and the result is also of the opposite sign.
  SetOrClearFlags(((original < 0) != (((int8_t)sub) < 0)) &&
                  ((original < 0) != (((int8_t)result) < 0)),
                  state->Registers, OVERFLOW_FLAG);
  SetOrClearFlags(sum >= 0, state->Registers, CARRY_FLAG);
  return result;
}

void SubWCarry(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = SubWCarryImpl(state,
                                              state->Registers->Accumulator,
                                              CalculateShortArgument(state, mode));
}

void And(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator &= CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void AShiftLeft(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t oldValue = CalculateShortArgument(state, mode);
  uint8_t newValue = oldValue << 1;

  if (mode == Accumulator)
  {
    state->Registers->Accumulator = newValue;
  }
  else
  {
    *ConvertAddress(state, CalculateMemoryLocation(state, mode)) = newValue; 
  }

  SetStatusFlags(state->Registers, newValue);
  SetOrClearFlags((oldValue >> 7) & CARRY_FLAG, state->Registers, CARRY_FLAG);
}

void ShiftLeftOr(struct MachineStructure * state, enum AddressMode mode)
{
  AShiftLeft(state, mode);
  IncOr(state, mode);
}

void ShiftRightExOr(struct MachineStructure * state, enum AddressMode mode)
{
  LShiftRight(state, mode);
  ExOr(state, mode);
}

void LShiftRight(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t oldValue = CalculateShortArgument(state, mode);
  uint8_t newValue = (oldValue >> 1) & 0x7F;
  
  if (mode == Accumulator)
  {
    state->Registers->Accumulator = newValue;
  }
  else
  {
    *ConvertAddress(state, CalculateMemoryLocation(state, mode)) = newValue;
  }

  SetStatusFlags(state->Registers, newValue);
  SetOrClearFlags(oldValue & CARRY_FLAG, state->Registers, CARRY_FLAG);
}

void RotateLeft(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t oldValue = CalculateShortArgument(state, mode);
  uint8_t newValue = (oldValue << 1) + (state->Registers->StatusFlags & CARRY_FLAG);

  if (mode == Accumulator)
  {
    state->Registers->Accumulator = newValue;
  }
  else
  {
    *ConvertAddress(state, CalculateMemoryLocation(state, mode)) = newValue;
  }

  SetStatusFlags(state->Registers, newValue);
  SetOrClearFlags((oldValue >> 7) & CARRY_FLAG, state->Registers, CARRY_FLAG);
}

void RotateRight(struct MachineStructure * state, enum AddressMode mode)
{
   uint8_t oldValue = CalculateShortArgument(state, mode);
   uint8_t newValue = ((oldValue >> 1) & 0x7F) + 
                      ((state->Registers->StatusFlags & CARRY_FLAG) << 7);

  if (mode == Accumulator)
  {
    state->Registers->Accumulator = newValue;
  }
  else
  {
    *ConvertAddress(state, CalculateMemoryLocation(state, mode)) = newValue;
  }

  SetStatusFlags(state->Registers, newValue);
  SetOrClearFlags(oldValue & CARRY_FLAG, state->Registers, CARRY_FLAG);
}

void RotateLeftAnd(struct MachineStructure * state, enum AddressMode mode)
{
  RotateLeft(state, mode);
  And(state, mode);
}

void RotateRightAdd(struct MachineStructure * state, enum AddressMode mode)
{
  RotateRight(state, mode);
  AddWCarry(state, mode);
}

void BitTest(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t argument = CalculateShortArgument(state, mode);
  uint8_t testValue = state->Registers->Accumulator & argument;
  SetOrClearFlags(testValue == 0, state->Registers, ZERO_FLAG);
  SetOrClearFlags(argument & OVERFLOW_FLAG, state->Registers, OVERFLOW_FLAG);
  SetOrClearFlags(argument & NEGATIVE_FLAG, state->Registers, NEGATIVE_FLAG);
}

void BranchCarryClear(struct MachineStructure * state, enum AddressMode mode)
{
  // TODO: Additional cycles for branch success.
  if (!(state->Registers->StatusFlags & CARRY_FLAG))
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchCarrySet(struct MachineStructure * state, enum AddressMode mode)
{
  // TODO: Additional cycles for branch success.
  if (state->Registers->StatusFlags & CARRY_FLAG)
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchOverflowClear(struct MachineStructure * state, enum AddressMode mode)
{
  if (!(state->Registers->StatusFlags & OVERFLOW_FLAG))
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchOverflowSet(struct MachineStructure * state, enum AddressMode mode)
{
  if (state->Registers->StatusFlags & OVERFLOW_FLAG)
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchEqual(struct MachineStructure * state, enum AddressMode mode)
{
  if (state->Registers->StatusFlags & ZERO_FLAG)
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchUnequal(struct MachineStructure * state, enum AddressMode mode)
{
  if (!(state->Registers->StatusFlags & ZERO_FLAG))
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchNeg(struct MachineStructure * state, enum AddressMode mode)
{
  if (state->Registers->StatusFlags & NEGATIVE_FLAG)
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void BranchPos(struct MachineStructure * state, enum AddressMode mode)
{
  if (!(state->Registers->StatusFlags & NEGATIVE_FLAG))
  {
    state->Registers->ProgramCounter += CalculateShortArgument(state, mode);
  }
}

void Break(struct MachineStructure * state, enum AddressMode mode)
{  // TODO
   PushStack16(state, state->Registers->ProgramCounter);
   PushStack8(state, state->Registers->StatusFlags);
   state->Registers->ProgramCounter = Read16(state, 0xFFFE);
   SetFlags(state->Registers, BREAK_FLAG);   
}

void SetCarry(struct MachineStructure * state, enum AddressMode mode)
{
  SetFlags(state->Registers, CARRY_FLAG);
}

void ClearCarry(struct MachineStructure * state, enum AddressMode mode)
{
  ClearFlags(state->Registers, CARRY_FLAG);
}

void SetDecimal(struct MachineStructure * state, enum AddressMode mode)
{
  SetFlags(state->Registers, DECIMAL_FLAG);
}

void ClearDecimal(struct MachineStructure * state, enum AddressMode mode)
{
  ClearFlags(state->Registers, DECIMAL_FLAG);
}

void SetInterDisable(struct MachineStructure * state, enum AddressMode mode)
{
  SetFlags(state->Registers, NOINTERUPT_FLAG);
}

void ClearInterDisable(struct MachineStructure * state, enum AddressMode mode)
{
  ClearFlags(state->Registers, NOINTERUPT_FLAG);
}

void ClearOverflow(struct MachineStructure * state, enum AddressMode mode)
{
  ClearFlags(state->Registers, OVERFLOW_FLAG);
}

void CompareValues(struct RegisterStructure * registers, uint8_t val1, uint8_t val2)
{
  SetStatusFlags(registers, val1 - val2);
  SetOrClearFlags(val1 >= val2, registers, CARRY_FLAG);
}

void Compare(struct MachineStructure * state, enum AddressMode mode)
{
  CompareValues(state->Registers,
                state->Registers->Accumulator,
                CalculateShortArgument(state, mode));
}

void CompareX(struct MachineStructure * state, enum AddressMode mode)
{  
  CompareValues(state->Registers,
                state->Registers->X,
                CalculateShortArgument(state, mode));
}

void CompareY(struct MachineStructure * state, enum AddressMode mode)
{
  CompareValues(state->Registers,
                state->Registers->Y,
                CalculateShortArgument(state, mode));
}

void IncOr(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator |= CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
  // TODO: Page cross check
}

void ExOr(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator ^= CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void Increment(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * memLocation = ConvertAddress(state, CalculateMemoryLocation(state, mode));
  SetStatusFlags(state->Registers, ++(*memLocation));
}

void IncrementX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X++;
  SetStatusFlags(state->Registers, state->Registers->X);
}

void IncrementY(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Y++;
  SetStatusFlags(state->Registers, state->Registers->Y);
}

void Decrement(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * memLocation = ConvertAddress(state, CalculateMemoryLocation(state, mode));
  SetStatusFlags(state->Registers, --(*memLocation));
}

void DecrementX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X--;
  SetStatusFlags(state->Registers, state->Registers->X);
}

void DecrementY(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Y--;
  SetStatusFlags(state->Registers, state->Registers->Y);
}

void DecCompare(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * memLocation = ConvertAddress(state, CalculateMemoryLocation(state, mode));
  uint8_t value = (*memLocation);
  --value;
  *memLocation = value;

  // Expected output does not actually do this. Illegal opcode anyway.
  CompareValues(state->Registers, state->Registers->Accumulator, value); 
}

void IncSubWCarry(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * memLocation = ConvertAddress(state,
                                         CalculateMemoryLocation(state, mode));
  uint8_t value = (*memLocation);
  ++value;
  *memLocation = value;

  state->Registers->Accumulator = SubWCarryImpl(state,
                                                state->Registers->Accumulator,
                                                value);
}

void Jump(struct MachineStructure * state, enum AddressMode mode)
{
   uint16_t newPCLocation = CalculateMemoryLocation(state, mode);
   if (mode == Indirect)
   {
     state->Registers->ProgramCounter = Read16WrapBug(state, newPCLocation);
   }
   else
   {
     state->Registers->ProgramCounter = Read16(state, newPCLocation);
   }

   // Subtract 3 from PC because it will be added on again later.
   state->Registers->ProgramCounter = state->Registers->ProgramCounter - 3;   
}

void StartMethod(struct MachineStructure * state, enum AddressMode mode)
{
  // Increment PC by 2 before pushing (length of JSR - length of RTS)
  PushStack16(state, state->Registers->ProgramCounter + 2);

  // Subtract opcode length because it will be added back on.
  state->Registers->ProgramCounter = CalculateMemoryLocation(state, mode) - 3;
}

void EndMethod(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->ProgramCounter = PopStack16(state);
  // Automatically incremented by 1
}

void EndInterrupt(struct MachineStructure * state, enum AddressMode mode)
{
  PullState(state, mode);
  // Will be automatically interrupted by 1 but paused mid-instruction.
  state->Registers->ProgramCounter = PopStack16(state) - 1;
}

void LoadAccum(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void LoadX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->X);
}

void LoadAccumX(struct MachineStructure * state, enum AddressMode mode)
{
  LoadX(state, mode);
  state->Registers->Accumulator = state->Registers->X;
}

void LoadY(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Y = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Y);
}

void PushAccum(struct MachineStructure * state, enum AddressMode mode)
{
  PushStack8(state, state->Registers->Accumulator);
}

void PushState(struct MachineStructure * state, enum AddressMode mode)
{
  // BREAK_FLAG indicates pushed with PHP command
  PushStack8(state, state->Registers->StatusFlags | BREAK_FLAG);
}

void PullAccum(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = PopStack8(state);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void PullState(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->StatusFlags = (PopStack8(state) & 0xEF) | UNUSED_FLAG;
}

void StoreAccum(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * address = ConvertAddress(state, CalculateMemoryLocation(state, mode));
  *address = state->Registers->Accumulator;
}

void StoreX(struct MachineStructure * state, enum AddressMode mode)
{
  *ConvertAddress(state, CalculateMemoryLocation(state, mode)) =
                  state->Registers->X;
}

void StoreAccumX(struct MachineStructure * state, enum AddressMode mode)
{
  *ConvertAddress(state, CalculateMemoryLocation(state, mode)) =
                  state->Registers->X & state->Registers->Accumulator;
}

void StoreY(struct MachineStructure * state, enum AddressMode mode)
{
  *ConvertAddress(state, CalculateMemoryLocation(state, mode)) =
                  state->Registers->Y;
}

void AccumToX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X = state->Registers->Accumulator;
  SetStatusFlags(state->Registers, state->Registers->X);
}

void AccumToY(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Y = state->Registers->Accumulator;
  SetStatusFlags(state->Registers, state->Registers->Y);
}

void SPToX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X = state->Registers->StackPointer;
  SetStatusFlags(state->Registers, state->Registers->X);
}

void XToSP(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->StackPointer = state->Registers->X;
}

void XToAccum(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = state->Registers->X;
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void YToAccum(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = state->Registers->Y;
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void NoOp(struct MachineStructure * state, enum AddressMode mode)
{
  // This function intentionally left blank.
}

int8_t CalculateShortArgument(struct MachineStructure * state, enum AddressMode mode)
{
  int8_t * argPtr = (int8_t*)ConvertAddress(state, state->Registers->ProgramCounter + 1);
  int8_t arg = *argPtr;
  int8_t * resultPtr = NULL;
  int8_t result = 0;

  switch (mode)
  {
    case Invalid:
    case Implied:
      // TODO: Error
      return 0;
    case Immediate:
      // TODO: Check conversion
      return arg;
    case Relative:
      return arg;
    case Accumulator:
      return state->Registers->Accumulator;
    case ZeroPage:
    case ZeroPageX:
    case ZeroPageY:
    case Absolute:
    case AbsoluteX:
    case AbsoluteY:
    case Indirect:
    case IndirectX:
    case IndirectY:
      resultPtr = ConvertAddress(state, CalculateMemoryLocation(state, mode));
      result = *resultPtr;
      return result;
    default:
      // TODO: Error
      return 0;
  }
}

uint16_t CalculateArgument(struct MachineStructure * state, enum AddressMode mode)
{
  unsigned char * argPtr = ConvertAddress(state, state->Registers->ProgramCounter + 1);
  unsigned char arg = *argPtr;
  unsigned char arg2 = *(argPtr + 1);

  switch (mode)
  {
    case Invalid:
    case Implied:
      // TODO: Error
      return 0;
    case Immediate:
      return ConvertFromLittleEndian(arg, arg2);
    case Relative:
      return (signed char)(arg);
    case Accumulator:
      return state->Registers->Accumulator;
    case ZeroPage:
    case ZeroPageX:
    case ZeroPageY:
    case Absolute:
    case AbsoluteX:
    case AbsoluteY:
    case Indirect:
    case IndirectX:
    case IndirectY:
      return Read16(state, CalculateMemoryLocation(state, mode));
    default:
      // TODO: Error
      return 0;
  }
}

uint16_t CalculateMemoryLocation(struct MachineStructure * state, enum AddressMode mode)
{
  uint8_t * argPtr = ConvertAddress(state, state->Registers->ProgramCounter + 1);
  uint8_t arg = *argPtr;
  uint8_t arg2 = *(argPtr + 1);
  uint16_t memLocation = 0;

  switch (mode)
  {
    case Invalid:
    case Implied:
    case Relative:
    case Accumulator:
      // TODO: Error
      return 0;
    case Immediate:
      return state->Registers->ProgramCounter + 1;
    case ZeroPage:
      return arg;
    case ZeroPageX:
      return 0xFF & (arg + state->Registers->X);
    case ZeroPageY:
      return 0xFF & (arg + state->Registers->Y);
    case Absolute:
      return ConvertFromLittleEndian(arg, arg2);
    case AbsoluteX:
      return ConvertFromLittleEndian(arg, arg2) + state->Registers->X;
    case AbsoluteY:
      return ConvertFromLittleEndian(arg, arg2) + state->Registers->Y;
    case Indirect:
      return Read16WrapBug(state, state->Registers->ProgramCounter + 1);
    case IndirectX:
      // Adding 8 bit numbers.
      return Read16WrapBug(state, 0x00FF & (arg + state->Registers->X));
    case IndirectY:
      return Read16WrapBug(state, arg) + state->Registers->Y;
    default:
      // TODO: Error
      return 0;
  }
}

uint16_t Read16WrapBug(struct MachineStructure * state, uint16_t LSBptr)
{
  uint8_t LSB = *ConvertAddress(state, LSBptr);
  uint16_t MSBptr = (LSBptr & 0xFF00) | ((LSBptr + 1) & 0x00FF);
  uint8_t MSB = *ConvertAddress(state, MSBptr);
  return ConvertFromLittleEndian(LSB, MSB);
}
