#include <stdio.h>

#include "Cpu.h"
#include "Structure.h"
#include "Utility.h"

#define DEBUG true

struct Operation Operations[256] =
{
  { "BRK", 1, 7, Implied, Break }, { "ORA", 2, 6, IndirectX, IncOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ORA", 2, 3, ZeroPage, IncOr },
  { "ASL", 2, 5, ZeroPage, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "PHP", 1, 3, Implied, PushState }, { "ORA", 2, 2, Immediate, IncOr },
  { "ASL", 1, 2, Accumulator, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ORA", 3, 4, Absolute, IncOr },
  { "ASL", 3, 6, Absolute, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "BPL", 2, 2, Relative, BranchPos }, { "ORA", 2, 5, IndirectY, IncOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ORA", 2, 4, ZeroPageX, IncOr },
  { "ASL", 2, 6, ZeroPageX, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "CLC", 1, 2, Implied, ClearCarry }, { "ORA", 3, 4, AbsoluteY, IncOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ORA", 3, 4, AbsoluteX, IncOr },
  { "ASL", 3, 7, AbsoluteX, AShiftLeft }, { "INV", 1, 1, Invalid, NULL },
  { "JSR", 3, 6, Absolute, StartMethod }, { "AND", 2, 6, IndirectX, And },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "BIT", 2, 3, ZeroPage, BitTest }, { "AND", 2, 3, ZeroPage, And },
  { "ROL", 2, 5, ZeroPage, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "PLP", 1, 4, Implied, PullState }, { "AND", 2, 2, Immediate, And },
  { "ROL", 1, 2, Accumulator, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "BIT", 3, 4, Absolute, BitTest }, { "AND", 3, 4, Absolute, And },
  { "ROL", 3, 6, Absolute, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "BMI", 2, 2, Relative, BranchNeg }, { "AND", 2, 5, IndirectY, And },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "AND", 2, 4, ZeroPageX, And },
  { "ROL", 2, 6, ZeroPageX, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "SEC", 1, 2, Implied, SetCarry }, { "AND", 3, 4, AbsoluteY, And },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "AND", 3, 4, AbsoluteX, And },
  { "ROL", 3, 7, AbsoluteX, RotateLeft }, { "INV", 1, 1, Invalid, NULL },
  { "RTI", 1, 6, Implied, EndInterrupt }, { "EOR", 2, 6, IndirectX, ExOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "EOR", 2, 3, ZeroPage, ExOr },
  { "LSR", 2, 5, ZeroPage, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  { "PHA", 1, 3, Implied, PushAccum }, { "EOR", 2, 2, Immediate, ExOr },
  { "LSR", 1, 2, Accumulator, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  // Officially, this is an Absolute Jump.
  { "JMP", 3, 3, Immediate, Jump }, { "EOR", 3, 4, Absolute, ExOr },
  { "LSR", 3, 6, Absolute, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  { "BVC", 2, 2, Relative, BranchOverflowClear }, { "EOR", 2, 5, IndirectY, ExOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "EOR", 2, 4, ZeroPageX, ExOr },
  { "LSR", 2, 6, ZeroPageX, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  { "CLI", 1, 2, Implied, ClearInterDisable }, { "EOR", 3, 4, AbsoluteY, ExOr },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "EOR", 3, 4, AbsoluteX, ExOr },
  { "LSR", 3, 7, AbsoluteX, LShiftRight }, { "INV", 1, 1, Invalid, NULL },
  { "RTS", 1, 6, Implied, EndMethod }, { "ADC", 2, 6, IndirectX, AddWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ADC", 2, 3, ZeroPage, AddWCarry },
  { "ROR", 2, 5, ZeroPage, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "PLA", 1, 4, Implied, PullAccum }, { "ADC", 2, 2, Immediate, AddWCarry },
  { "ROR", 1, 2, Accumulator, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "JMP", 3, 5, Indirect, Jump }, { "ADC", 3, 4, Absolute, AddWCarry },
  { "ROR", 3, 6, Absolute, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "BVS", 2, 2, Relative, BranchOverflowSet }, { "ADC", 2, 5, IndirectY, AddWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ADC", 2, 4, ZeroPageX, AddWCarry },
  { "ROR", 2, 6, ZeroPageX, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "SEI", 1, 2, Implied, SetInterDisable }, { "ADC", 3, 4, AbsoluteY, AddWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "ADC", 3, 4, AbsoluteX, AddWCarry },
  { "ROR", 3, 7, AbsoluteX, RotateRight }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "STA", 2, 6, IndirectX, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "STY", 2, 3, ZeroPage, StoreY }, { "STA", 2, 3, ZeroPage, StoreAccum },
  { "STX", 2, 3, ZeroPage, StoreX }, { "INV", 1, 1, Invalid, NULL },
  { "DEY", 1, 2, Implied, DecrementY }, { "INV", 1, 1, Invalid, NULL },
  { "TXA", 1, 2, Implied, XToAccum }, { "INV", 1, 1, Invalid, NULL },
  { "STY", 3, 4, Absolute, StoreY }, { "STA", 3, 4, Absolute, StoreAccum },
  { "STX", 3, 4, Absolute, StoreX }, { "INV", 1, 1, Invalid, NULL },
  { "BCC", 2, 2, Relative, BranchCarryClear }, { "STA", 2, 6, IndirectY, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "STY", 2, 4, ZeroPageX, StoreY }, { "STA", 2, 4, ZeroPageX, StoreAccum },
  { "STX", 2, 4, ZeroPageY, StoreX }, { "INV", 1, 1, Invalid, NULL },
  { "TYA", 1, 2, Implied, YToAccum }, { "STA", 3, 5, AbsoluteY, StoreAccum },
  { "TXS", 1, 2, Implied, XToSP }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "STA", 3, 5, AbsoluteX, StoreAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 2, 2, Immediate, LoadY }, { "LDA", 2, 6, IndirectX, LoadAccum },
  { "LDX", 2, 2, Immediate, LoadX }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 2, 3, ZeroPage, LoadY }, { "LDA", 2, 3, ZeroPage, LoadAccum },
  { "LDX", 2, 3, ZeroPage, LoadX }, { "INV", 1, 1, Invalid, NULL },
  { "TAY", 1, 2, Implied, AccumToY }, { "LDA", 2, 2, Immediate, LoadAccum },
  { "TAX", 1, 2, Implied, AccumToX }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 3, 4, Absolute, LoadY }, { "LDA", 3, 4, Absolute, LoadAccum },
  { "LDX", 3, 4, Absolute, LoadX }, { "INV", 1, 1, Invalid, NULL },
  { "BCS", 2, 2, Relative, BranchCarrySet }, { "LDA", 2, 5, IndirectY, LoadAccum },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 2, 4, ZeroPageX, LoadY }, { "LDA", 2, 4, ZeroPageX, LoadAccum },
  { "LDX", 2, 4, ZeroPageY, LoadX }, { "INV", 1, 1, Invalid, NULL },
  { "CLV", 1, 2, Implied, ClearOverflow }, { "LDA", 3, 4, AbsoluteY, LoadAccum },
  { "TSX", 1, 2, Implied, SPToX }, { "INV", 1, 1, Invalid, NULL },
  { "LDY", 3, 4, AbsoluteX, LoadY }, { "LDA", 3, 4, AbsoluteX, LoadAccum },
  { "LDX", 3, 4, AbsoluteY, LoadX }, { "INV", 1, 1, Invalid, NULL },
  { "CPY", 2, 2, Immediate, CompareY }, { "CMP", 2, 6, IndirectX, Compare },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "CPY", 2, 3, ZeroPage, CompareY }, { "CMP", 2, 3, ZeroPage, Compare },
  { "DEC", 2, 5, ZeroPage, Decrement }, { "INV", 1, 1, Invalid, NULL },
  { "INY", 1, 2, Implied, IncrementY }, { "CMP", 2, 2, Immediate, Compare },
  { "DEX", 1, 2, Implied, DecrementX }, { "INV", 1, 1, Invalid, NULL },
  { "CPY", 3, 4, Absolute, CompareY }, { "CMP", 3, 4, Absolute, Compare },
  { "DEC", 3, 6, Absolute, Decrement }, { "INV", 1, 1, Invalid, NULL },
  { "BNE", 2, 2, Relative, BranchUnequal }, { "CMP", 2, 5, IndirectY, Compare },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "CMP", 2, 4, ZeroPageX, Compare },
  { "DEC", 2, 6, ZeroPageX, Decrement }, { "INV", 1, 1, Invalid, NULL },
  { "CLD", 1, 2, Implied, ClearDecimal }, { "CMP", 3, 4, AbsoluteY, Compare },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "CMP", 3, 4, AbsoluteX, Compare },
  { "DEC", 3, 7, AbsoluteX, Decrement }, { "INV", 1, 1, Invalid, NULL },
  { "CPX", 2, 2, Immediate, CompareX }, { "SBC", 2, 6, IndirectX, SubWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "CPX", 2, 3, ZeroPage, CompareX }, { "SBC", 2, 3, ZeroPage, SubWCarry },
  { "INC", 2, 5, ZeroPage, Increment }, { "INV", 1, 1, Invalid, NULL },
  { "INX", 1, 2, Implied, IncrementX }, { "SBC", 2, 2, Immediate, SubWCarry },
  { "NOP", 1, 2, Implied, NoOp }, { "INV", 1, 1, Invalid, NULL },
  { "CPX", 3, 4, Absolute, CompareX }, { "SBC", 3, 4, Absolute, SubWCarry },
  { "INC", 3, 6, Absolute, Increment }, { "INV", 1, 1, Invalid, NULL },
  { "BEQ", 2, 2, Relative, BranchEqual }, { "SBC", 2, 5, IndirectY, SubWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "SBC", 2, 4, ZeroPageX, SubWCarry },
  { "INC", 2, 6, ZeroPageX, Increment }, { "INV", 1, 1, Invalid, NULL },
  { "SED", 1, 2, Implied, SetDecimal }, { "SBC", 3, 4, AbsoluteY, SubWCarry },
  { "INV", 1, 1, Invalid, NULL }, { "INV", 1, 1, Invalid, NULL },
  { "INV", 1, 1, Invalid, NULL }, { "SBC", 3, 4, AbsoluteX, SubWCarry },
  { "INC", 3, 7, AbsoluteX, Increment }, { "INV", 1, 1, Invalid, NULL }
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

void SetStatusFlagsWithSign(struct RegisterStructure * registers, int8_t value, int16_t expected)
{
  SetStatusFlags(registers, value);
  SetOrClearFlags((registers->StatusFlags & NEGATIVE_FLAG) != (expected < 0),
    registers, OVERFLOW_FLAG);
}

void AddWCarry(struct MachineStructure * state, enum AddressMode mode)
{
  int16_t sum = state->Registers->Accumulator;
  sum += CalculateShortArgument(state, mode);
  sum += (state->Registers->StatusFlags & CARRY_FLAG);
  state->Registers->Accumulator = (int8_t)sum;
  
  SetStatusFlagsWithSign(state->Registers, state->Registers->Accumulator, sum);
  SetOrClearFlags(sum > state->Registers->Accumulator, state->Registers, CARRY_FLAG);
}

void SubWCarry(struct MachineStructure * state, enum AddressMode mode)
{
  int16_t sum = state->Registers->Accumulator;
  sum -= CalculateShortArgument(state, mode);
  sum -= (1 - (state->Registers->StatusFlags & CARRY_FLAG));
  state->Registers->Accumulator = (int8_t)sum;

  SetStatusFlagsWithSign(state->Registers, state->Registers->Accumulator, sum);
  if (sum > (state->Registers->Accumulator & 0x7F))
  {
    SetFlags(state->Registers, CARRY_FLAG);
  }
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
   uint8_t newValue = ((newValue >> 1) & 0x7F) + 
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
  if (val2 >= val1)
  {
    SetFlags(registers, CARRY_FLAG);
  }
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

void Jump(struct MachineStructure * state, enum AddressMode mode)
{
   // Subtract opcode length because it will be added back on.
   state->Registers->ProgramCounter = CalculateArgument(state, mode) - 3;
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
{  // TODO
}

void LoadAccum(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Accumulator = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void LoadX(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->X = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
}

void LoadY(struct MachineStructure * state, enum AddressMode mode)
{
  state->Registers->Y = CalculateShortArgument(state, mode);
  SetStatusFlags(state->Registers, state->Registers->Accumulator);
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
  *ConvertAddress(state, CalculateMemoryLocation(state, mode)) =
                  state->Registers->Accumulator;
}

void StoreX(struct MachineStructure * state, enum AddressMode mode)
{
  *ConvertAddress(state, CalculateMemoryLocation(state, mode)) =
                  state->Registers->X;
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
      return *ConvertAddress(state, CalculateMemoryLocation(state, mode));
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
      // TODO: Check conversion
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
  unsigned char * argPtr = ConvertAddress(state, state->Registers->ProgramCounter + 1);
  unsigned char arg = *argPtr;
  unsigned char arg2 = *(argPtr + 1);
  unsigned char * memLocation = NULL;

  switch (mode)
  {
    case Invalid:
    case Implied:
    case Immediate:
    case Relative:
    case Accumulator:
      // TODO: Error
      return 0;
    case ZeroPage:
      return arg;
    case ZeroPageX:
      return arg + state->Registers->X;
    case ZeroPageY:
      return arg + state->Registers->Y;
    case Absolute:
      return ConvertFromLittleEndian(arg, arg2);
    case AbsoluteX:
      return ConvertFromLittleEndian(arg, arg2) + state->Registers->X;
    case AbsoluteY:
      return ConvertFromLittleEndian(arg, arg2) + state->Registers->Y;
    case Indirect:
      memLocation = ConvertAddress(state, ConvertFromLittleEndian(arg, arg2));
      return ConvertFromLittleEndian(*memLocation, *(memLocation + 1));
    case IndirectX:
      memLocation = ConvertAddress(state, arg + state->Registers->X);
      return *ConvertAddress(state, ConvertFromLittleEndian(*memLocation, *(memLocation + 1)));
    case IndirectY:
      return *ConvertAddress(state, ConvertFromLittleEndian(arg, arg+1)) + state->Registers->Y;
    default:
      // TODO: Error
      return 0;
  }
}
