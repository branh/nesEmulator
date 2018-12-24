#ifndef _UTILITY_H_
#define _UTILITY_H_

#include "KernelCalls.h"

#include <stdbool.h>
#include <stddef.h>

void DiagnosticTrace(const char * message);
bool ReadAndVerify(void * dest, size_t offset, size_t length, struct File * file);

bool CompareHexStringToInt(size_t value, char * hexString);
char * AdvanceToNextWhitespace(char *);

#endif
