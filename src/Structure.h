#ifndef NES_STRUCTURE_H
#define NES_STRUCTURE_H

#include "Rom.h"

#include <stdint.h>

struct RegisterStructure
{
  uint16_t ProgramCounter; // Check Types
  uint8_t StackPointer;
  uint8_t StatusFlags;
  uint8_t Accumulator;
  uint8_t X;
  uint8_t Y;
};

struct MachineStructure
{
  struct RegisterStructure * Registers;
  struct Rom * Rom;

  unsigned char * Memory;
  int CycleCount; // TODO: Decide type or even what to do with this.
};

#define CARRY_FLAG 0x01
#define ZERO_FLAG 0x02
#define NOINTERUPT_FLAG 0x04
#define DECIMAL_FLAG 0x08
#define BREAK_FLAG 0x10
#define UNUSED_FLAG 0x20
#define OVERFLOW_FLAG 0x40
#define NEGATIVE_FLAG 0x80

void SetOrClearFlags(uint8_t condition, struct RegisterStructure *, uint8_t);
void ClearFlags(struct RegisterStructure * state, uint8_t clearFlags);
void SetFlags(struct RegisterStructure * state, uint8_t newFlags);
uint16_t ConvertFromLittleEndian(uint8_t LSB, uint8_t MSB);

void PushStack8(struct MachineStructure *, uint8_t);
void PushStack16(struct MachineStructure *, uint16_t);
uint8_t PopStack8(struct MachineStructure *);
uint16_t PopStack16(struct MachineStructure *);
uint16_t Read16(struct MachineStructure *, uint16_t LSBptr);

uint8_t * CalculateStackPointer(struct MachineStructure*);
uint8_t * ConvertAddress(struct MachineStructure *, uint16_t);

struct MachineStructure * ConstructMachineState();
void LoadRom(struct MachineStructure *, struct Rom *);
void DisposeMachineState(struct MachineStructure *);

#endif
