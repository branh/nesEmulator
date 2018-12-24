#include "Cpu.h"
#include "CLI.h"
#include "Rom.h"
#include "Structure.h"

#include <stdio.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH 100

int main(int argc, char ** argv)
{
  struct Arguments args = ProcessArguments(argc, argv);
  if (args.Valid == 0)
  {
    PrintUsage();
    return 0;
  }

  struct File * compareFile = NULL;
  char * expectedLine = NULL;
  if (args.Compare)
  {
    compareFile = OpenFile(args.CompareFile, true, true, false);
    expectedLine = malloc(sizeof(char) * MAX_LINE_LENGTH);
  }

  struct MachineStructure * machine = ConstructMachineState();
  struct Rom * rom = ReadRom(args.RomFile);
  if (machine && rom)
  {
    bool noBugs = true;

    LoadRom(machine, rom);
    
    // Start test PC location.
    machine->Registers->ProgramCounter = 0xC000;
    while (noBugs && ReadLine(compareFile, expectedLine, MAX_LINE_LENGTH) == true)
    {
      noBugs = noBugs && ProcessInstruction(machine, expectedLine, args.Log || args.Compare, args.Compare);
    }
  }
  else
  {
    printf("Failed to load rom.");
  }

  DisposeMachineState(machine);
  if (compareFile)
  {
    CloseFile(compareFile);
    free(expectedLine);
  }
}
