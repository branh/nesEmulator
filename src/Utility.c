#include "Utility.h"

#include <stdio.h> 
#include <string.h>

void DiagnosticTrace(const char * message)
{
  printf("%s\n", message);
}

bool ReadAndVerify(void * dest, size_t offset, size_t length, struct File * file)
{
  size_t lengthRead = ReadFile(file, dest, offset, length);
  return length == length;
}

bool CompareHexStringToInt(size_t value, char * hexString)
{
  if (!hexString)
  {
    DiagnosticTrace("Comparing null string.");
    return false;
  }

  size_t hexVal = 0;
  size_t index = 0;
  char * hexValues = "0123456789ABCDEF";
  char * curValue;
  while ((curValue = strchr(hexValues, hexString[index])) && *curValue != '\0')
  {
    hexVal = (hexVal * 16) + (curValue - hexValues);
    ++hexString;
  }
  return value == hexVal;
}

char * AdvanceToNextWhitespace(char * str)
{
  while (*str == ' ')
  {
    ++str;
  }
  return str;
}
