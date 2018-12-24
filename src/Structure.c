#include "Structure.h"
#include "Utility.h"

#include <stdint.h>
#include <stdlib.h>

void SetOrClearFlags(uint8_t condition,
                     struct RegisterStructure * state, uint8_t flags)
{
  if (condition)
  {
    SetFlags(state, flags);
  }
  else
  {
    ClearFlags(state, flags);
  }
}

void ClearFlags(struct RegisterStructure * state, uint8_t clearFlags)
{
  state->StatusFlags = state->StatusFlags & ~clearFlags;
}

void SetFlags(struct RegisterStructure* state, uint8_t newFlags)
{
  state->StatusFlags = state->StatusFlags | newFlags;
}

uint16_t ConvertFromLittleEndian(uint8_t LSB, uint8_t MSB)
{
  uint16_t converted = MSB;
  converted = (converted << 8) + LSB;
  return converted;
}

uint16_t Read16(struct MachineStructure * state, uint16_t LSBptr)
{
  uint8_t * LSB = ConvertAddress(state, LSBptr);
  return ConvertFromLittleEndian(*LSB, *(LSB + 1));
}

void PushStack8(struct MachineStructure * state, uint8_t value)
{
  uint8_t * SP = CalculateStackPointer(state);
  *SP = value;
  state->Registers->StackPointer--;
}

void PushStack16(struct MachineStructure * state, uint16_t value)
{
  uint8_t hiVal = (uint8_t)(value / 0x100);
  uint8_t loVal = (uint8_t)(value % 0x100);
  PushStack8(state, hiVal);
  PushStack8(state, loVal);
}

uint8_t PopStack8(struct MachineStructure * state)
{
  state->Registers->StackPointer++;
  uint8_t value = *CalculateStackPointer(state);
}

uint16_t PopStack16(struct MachineStructure * state)
{
  uint8_t loVal = PopStack8(state);
  uint8_t hiVal = PopStack8(state);
  return ConvertFromLittleEndian(loVal, hiVal);
}

uint8_t* CalculateStackPointer(struct MachineStructure * state)
{
  return ConvertAddress(state, 0x0100 + state->Registers->StackPointer);
}

unsigned char * ConvertAddress(struct MachineStructure * state, uint16_t address)
{
  if (address >= 0xC000)
  {
    return GetProgBankC(state->Rom) + (address - 0xC000);
  }
  if (address >= 0x8000)
  {
    // TODO: Handle single prog bank.
    return GetProgBank8(state->Rom) + (address - 0x8000);
  }

  if (address < 0x2000)
  {
    return state->Memory + (address % 0x0800);
  }

  // TODO: Mirroring of 0x2000 to 0x4000
  // TODO: 0x4000 to 0x8000
  return NULL;
}

struct MachineStructure * ConstructMachineState()
{
  struct MachineStructure * state = (struct MachineStructure *)malloc(
         sizeof(struct MachineStructure));
  if (!state)
  {
    DiagnosticTrace("Could not allocate machine state.");
    return NULL;
  }
  state->CycleCount = 0;

  state->Registers = (struct RegisterStructure *)malloc(
    sizeof(struct RegisterStructure));
  if (!state->Registers)
  {
    DiagnosticTrace("Could not allocate registers.");
    DisposeMachineState(state);
    return NULL;
  }
  state->Registers->ProgramCounter = 0xFFFD;
  state->Registers->StackPointer = 0xFD;
  state->Registers->Accumulator = 0;
  state->Registers->X = 0;
  state->Registers->Y = 0;

  state->Memory = (unsigned char *)calloc(0x0800, 1);
  if (!state->Memory)
  {
    DiagnosticTrace("Could not allocate memory.");
    DisposeMachineState(state);
    return NULL;
  }

  state->Rom = NULL;
  return state;
}

void LoadRom(struct MachineStructure * state, struct Rom * rom)
{
  if (state->Rom)
  {
    DisposeRom(state->Rom);
  }
  state->Rom = rom;

  state->Registers->StackPointer = 0xFD;
  state->Registers->StatusFlags = 0x24;
  // TODO: Figure out why starting address is not 0xFFFC

  if (rom)
  {
    unsigned char * pcAddress = ConvertAddress(state, 0xFFFD);
    state->Registers->ProgramCounter =
      ConvertFromLittleEndian(*pcAddress, *(pcAddress + 1)); 
  }
}

void DisposeMachineState(struct MachineStructure * state)
{
  if (!state) 
  {
    return;
  }
  if (state->Registers)
  {
    free(state->Registers);
  }

  if (state->Memory)
  {
    free(state->Memory);
  }

  if (state->Rom)
  {
    DisposeRom(state->Rom);
  }
  free(state);
}
