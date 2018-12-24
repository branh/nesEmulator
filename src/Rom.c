#include "Rom.h"
#include "Utility.h"
#include "KernelCalls.h"

#include <stdlib.h>
#include <stdio.h>

#define HEADER_LENGTH 16

struct Rom * ReadRom(char * fileName)
{
  struct File * filePtr = OpenFile(fileName, true, false, false);
  if (!filePtr)
  {
    return NULL;
  }

  DiagnosticTrace("Opened file.");
  struct RomHeader * header = ReadRomHeader(filePtr);
  if (!header)
  {
    CloseFile(filePtr);
    return NULL;
  }

  DiagnosticTrace("Read valid rom");
  struct Rom * rom = (struct Rom *)malloc(sizeof(struct Rom));
  if (!rom)
  {
    free(header);
    CloseFile(filePtr);
    return NULL;
  }
  rom->Header = header;
  rom->File = filePtr;

  // For now, assume no trainer

  printf("Prog rom size is %d\n", rom->Header->ProgRomSize);
  printf("Mapper type is %x\n", rom->Header->MapperFlag);
  
  rom->MemMap = MapMemory(rom->File, 0,
                         (rom->Header->ProgRomSize * PROGROM_UNIT) + HEADER_LENGTH);
  rom->ProgRom = ((unsigned char *)rom->MemMap) + HEADER_LENGTH;
  if (!rom->MemMap)
  {
    DiagnosticTrace("ProgRom is empty.");
    DisposeRom(rom);
    return NULL;
  }

  rom->MapInfo = CalculateMapInfo(rom);
  if (!rom->MapInfo)
  {
    DiagnosticTrace("Could not calculate mapper info.");
  }

  // For now, ignore chrRom
  rom->ChrRom = NULL;
  return rom;
}

bool IsValidRomHeader(struct RomHeader * header)
{
  return (header->StartFlags[1] == 0x45 || header->StartFlags[0] == 0x4E ||
      header->StartFlags[3] == 0x1a || header->StartFlags[2] == 0x53);
}

struct RomHeader* ReadRomHeader(struct File* filePtr)
{
  if (!filePtr)
  {
    DiagnosticTrace("Attempting to read Rom from invalid file.");
    return NULL;
  }

  struct RomHeader* header = (struct RomHeader*)malloc(sizeof(struct RomHeader));
  if (!header)
  {
    return NULL;
  }

  if (!ReadAndVerify(header->StartFlags, 0, 4, filePtr) ||
    !ReadAndVerify(&(header->ChrRomSize), 4, 1, filePtr) ||
    !ReadAndVerify(&(header->ProgRomSize), 5, 1, filePtr) ||
    !ReadAndVerify(&(header->PlayChoiceFlags), 6, 1, filePtr) ||
    !ReadAndVerify(&(header->MapperFlag), 7, 1, filePtr) ||
    !ReadAndVerify(&(header->DoubleProgRomSize), 9, 1, filePtr) ||
    !IsValidRomHeader(header))
  {
    DiagnosticTrace("Invalid header!!");
    // TODO: Error
    free(header);
    return NULL;
  }

  return header;
}

void * CalculateMapInfo(struct Rom * rom)
{
  if (!rom)
  {
    DiagnosticTrace("Calculating map info for null rom!");
    return NULL;
  }

  rom->MapperType = (rom->Header->MapperFlag >> 4) & 0xFF;
  switch(rom->MapperType)
  {
    case 0:
      rom->Block8 = rom->ProgRom;
      rom->BlockC = rom->ProgRom + ((rom->Header->ProgRomSize - 1) * PROGROM_UNIT);
      // NROM has no mapper.
    case 1:
    {
      struct MMC1MapInfo * mapInfo = (struct MMC1MapInfo *)malloc(
                                      sizeof(struct MMC1MapInfo));
      for (unsigned char bank = 0;
           bank < 8 && bank < rom->Header->ProgRomSize; ++bank)
      {
        mapInfo->ProgRomBanks[bank] = rom->ProgRom + (PROGROM_UNIT * bank);
      }
      mapInfo->CurrentProgBank = mapInfo->ProgRomBanks[0];
      return mapInfo;
    }
    default:
    {
      DiagnosticTrace("Mapper not yet supported.");
      return NULL;
    }
  }
}

unsigned char * GetProgBank8(struct Rom * rom)
{
  if (!rom)
  {
    DiagnosticTrace("Getting Prog bank of null rom.");
    return NULL;
  }

  switch(rom->MapperType)
  {
    case 0:
      return rom->Block8; 
    case 1:
    default:
      DiagnosticTrace("Getting prog bank of unsupported mapper.");
      return NULL;
  }
}

unsigned char * GetProgBankC(struct Rom * rom)
{
  if (!rom)
  {
    DiagnosticTrace("Getting C Prog bank of null rom.");
  }

  switch(rom->MapperType)
  {
    case 0:
      return rom->BlockC;
    default:
      DiagnosticTrace("Getting prog bank of unsupported mapper.");
      return NULL;
  }
}

void DisposeRom(struct Rom * rom)
{
  if (rom)
  {
    UnmapMemory(rom->MemMap,
                (rom->Header->ProgRomSize * KILOBYTE) + HEADER_LENGTH);
    CloseFile(rom->File);
    free(rom->MapInfo);
    free(rom->Header);
    free(rom->ChrRom);
    free(rom);
  }
}
