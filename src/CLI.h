#ifndef _CLI_H_
#define _CLI_H_

#include <stdbool.h>

struct Arguments
{
  bool Log;
  bool Compare;
  bool Valid;
  char * RomFile;
  char * CompareFile;
};

void PrintUsage();
struct Arguments ProcessArguments(int args, char ** argv);

#endif 
