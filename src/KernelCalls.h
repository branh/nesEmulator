#ifndef _KERNEL_CALLS_H_
#define _KERNEL_CALLS_H_

#include <stdbool.h>
#include <stddef.h>

struct File;

struct File * OpenFile(const char * fileName, bool read, bool write, bool execute);
long ReadFile(struct File * file, void * buffer, size_t offset, size_t length);
bool ReadLine(struct File * file, char * line, size_t maxLength);
size_t GetFileSize(struct File *);
void CloseFile(struct File * file);

unsigned char * MapMemory(struct File * file, size_t offset, size_t length);
int UnmapMemory(unsigned char * address, size_t length);

#endif
