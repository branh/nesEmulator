#ifndef ROM_H
#define ROM_H

#include "KernelCalls.h"
#include <stdbool.h>

#define KILOBYTE 0x4000
#define PROGROM_UNIT (16 * KILOBYTE)

struct RomHeader
{
  unsigned char StartFlags[4];
  unsigned char ProgRomSize;
  unsigned char ChrRomSize;
  unsigned char MapperFlag;
  unsigned char PlayChoiceFlags;
  unsigned char DoubleProgRomSize;
  unsigned char Ignored[8];
};

struct Rom
{
  struct RomHeader* Header;
  struct File * File;
  unsigned char * ProgRom;
  unsigned char * ChrRom;

  unsigned char MapperType;
  unsigned char * Block8;
  unsigned char * BlockC;

  void * MapInfo;
  void * MemMap;
};

struct MMC1MapInfo
{
  unsigned char * ProgRomBanks[8];
  // TODO: ChrRom banks.

  unsigned char * CurrentProgBank;
};

struct Rom * ReadRom(char * fileName);
struct RomHeader * ReadRomHeader(struct File * filePtr);
void * CalculateMapInfo(struct Rom *);

void DisposeRom(struct Rom *);
bool IsValidRomHeader(struct RomHeader *);

unsigned char * GetProgBank8(struct Rom *);
unsigned char * GetProgBankC(struct Rom *);

#endif
