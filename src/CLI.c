#include "CLI.h"

#include <stdio.h>
#include <string.h>

void PrintUsage()
{
  printf("-L(og) <testfile> to log output\n");
  printf("-C(ompare) <testfile> <comparefile> to compare log to test file.\n");
}

struct Arguments ProcessArguments(int args, char ** argv)
{
  struct Arguments arguments;
  arguments.Valid = args > 1;
  arguments.Log = (args > 1) && (strlen(argv[1]) >= 2) && (argv[1][0] == '-')
                  && (argv[1][1] == 'L' || argv[1][1] == 'l');
  arguments.Compare = (args > 1 && strlen(argv[1]) >=2 && argv[1][0] == '-' &&
      (argv[1][1] == 'C' || argv[1][1] == 'c'));

  if (args > 2)
  {
    arguments.RomFile = argv[2];
  }

  if (arguments.Compare && args > 3)
  {
    arguments.CompareFile = argv[3];
  }

  if (arguments.RomFile == NULL ||
      (arguments.Compare && arguments.CompareFile == NULL))
  {
    arguments.Valid = false;
  }
  return arguments;
}
