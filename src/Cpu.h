#ifndef CPU_H
#define CPU_H

#include "Structure.h"

#include <stdint.h>

enum AddressMode
{
   Invalid, Implied, Immediate, Relative, Accumulator,
   ZeroPage, ZeroPageX, ZeroPageY,
   Absolute, AbsoluteX, AbsoluteY,
   Indirect, IndirectX, IndirectY
};

struct Operation
{
   char Name[4];
   unsigned char Length;
   unsigned char Cycles;
   enum AddressMode Mode;
   void (*Func) (struct MachineStructure *, enum AddressMode);
};

int8_t CalculateShortArgument(struct MachineStructure *, enum AddressMode);
uint16_t CalculateArgument(struct MachineStructure *, enum AddressMode);
uint16_t CalculateMemoryLocation(struct MachineStructure *, enum AddressMode);
uint16_t Read16WrapBug(struct MachineStructure *, uint16_t);

void CompareValues(struct RegisterStructure *, uint8_t, uint8_t);
void SetStatusFlags(struct RegisterStructure *, uint8_t);
uint8_t SubWCarryImpl(struct MachineStructure *, uint8_t, uint8_t);

unsigned char PrintInstruction(unsigned char * instruction);
void LogInstruction(unsigned char * instruction, struct MachineStructure *);
bool CompareInstruction(unsigned char * instruction, struct MachineStructure *, char * expected);
bool DebugInstruction(unsigned char * instruction, struct MachineStructure *,
                      char * expected, bool log, bool compare);
bool ProcessInstruction(struct MachineStructure * machineState, char * expected,
                        bool log, bool compare);


// Functions for specific operations.
void AddWCarry(struct MachineStructure *, enum AddressMode);
void SubWCarry(struct MachineStructure *, enum AddressMode);
void And(struct MachineStructure *, enum AddressMode);
void AShiftLeft(struct MachineStructure *, enum AddressMode);
void LShiftRight(struct MachineStructure *, enum AddressMode);
void RotateLeft(struct MachineStructure *, enum AddressMode);
void RotateRight(struct MachineStructure *, enum AddressMode);
void BitTest(struct MachineStructure *, enum AddressMode);
void BranchCarryClear(struct MachineStructure *, enum AddressMode);
void BranchCarrySet(struct MachineStructure *, enum AddressMode);
void BranchOverflowClear(struct MachineStructure *, enum AddressMode);
void BranchOverflowSet(struct MachineStructure *, enum AddressMode);
void BranchEqual(struct MachineStructure *, enum AddressMode);
void BranchNeg(struct MachineStructure *, enum AddressMode);
void BranchPos(struct MachineStructure *, enum AddressMode);
void BranchUnequal(struct MachineStructure *, enum AddressMode);
void Break(struct MachineStructure *, enum AddressMode);
void SetCarry(struct MachineStructure *, enum AddressMode);
void ClearCarry(struct MachineStructure *, enum AddressMode);
void SetDecimal(struct MachineStructure *, enum AddressMode);
void ClearDecimal(struct MachineStructure *, enum AddressMode);
void SetInterDisable(struct MachineStructure *, enum AddressMode);
void ClearInterDisable(struct MachineStructure *, enum AddressMode);
void ClearOverflow(struct MachineStructure *, enum AddressMode);
void Compare(struct MachineStructure *, enum AddressMode);
void CompareX(struct MachineStructure *, enum AddressMode);
void CompareY(struct MachineStructure *, enum AddressMode);
void IncOr(struct MachineStructure *, enum AddressMode);
void ExOr(struct MachineStructure *, enum AddressMode);
void Decrement(struct MachineStructure *, enum AddressMode);
void DecrementX(struct MachineStructure *, enum AddressMode);
void DecrementY(struct MachineStructure *, enum AddressMode);
void Increment(struct MachineStructure *, enum AddressMode);
void IncrementX(struct MachineStructure *, enum AddressMode);
void IncrementY(struct MachineStructure *, enum AddressMode);
void Jump(struct MachineStructure *, enum AddressMode);
void StartMethod(struct MachineStructure *, enum AddressMode);
void EndMethod(struct MachineStructure *, enum AddressMode);
void EndInterrupt(struct MachineStructure *, enum AddressMode);
void LoadAccum(struct MachineStructure *, enum AddressMode);
void LoadX(struct MachineStructure *, enum AddressMode);
void LoadY(struct MachineStructure *, enum AddressMode);
void PushAccum(struct MachineStructure *, enum AddressMode);
void PushState(struct MachineStructure *, enum AddressMode);
void PullAccum(struct MachineStructure *, enum AddressMode);
void PullState(struct MachineStructure *, enum AddressMode);
void StoreAccum(struct MachineStructure *, enum AddressMode);
void StoreX(struct MachineStructure *, enum AddressMode);
void StoreY(struct MachineStructure *, enum AddressMode);
void AccumToX(struct MachineStructure *, enum AddressMode);
void AccumToY(struct MachineStructure *, enum AddressMode);
void SPToX(struct MachineStructure *, enum AddressMode);
void XToAccum(struct MachineStructure *, enum AddressMode);
void YToAccum(struct MachineStructure *, enum AddressMode);
void XToSP(struct MachineStructure *, enum AddressMode);
void NoOp(struct MachineStructure *, enum AddressMode);

// Invalid instructions:
void LoadAccumX(struct MachineStructure *, enum AddressMode);
void StoreAccumX(struct MachineStructure *, enum AddressMode);
void DecCompare(struct MachineStructure *, enum AddressMode);
void IncSubWCarry(struct MachineStructure *, enum AddressMode);
void ShiftLeftOr(struct MachineStructure *, enum AddressMode);
void ShiftRightExOr(struct MachineStructure *, enum AddressMode);
void RotateLeftAnd(struct MachineStructure *, enum AddressMode);
void RotateRightAdd(struct MachineStructure *, enum AddressMode);
#endif
