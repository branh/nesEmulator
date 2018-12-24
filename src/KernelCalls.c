#include "KernelCalls.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

struct File
{
  FILE * fileDescriptor;
};

struct File * OpenFile(const char * fileName, bool read, bool write, bool execute)
{
  struct File * file = (struct File *)malloc(sizeof(struct File));
  if (!file)
  {
    return file;
  }

  if (read && write)
  {
    file->fileDescriptor = fopen(fileName, "rw");
  }
  else if (write)
  {
    file->fileDescriptor = fopen(fileName, "w");
  }
  else if (read)
  {
    file->fileDescriptor = fopen(fileName, "r");
  }

  if (file->fileDescriptor == NULL)
  {
    free(file);
    file = NULL;
  }
  return file;
}

long ReadFile(struct File * file, void * buffer, size_t offset, size_t count)
{
  if (!file || !file->fileDescriptor)
  {
    return -1;
  }
  fseek(file->fileDescriptor, offset, SEEK_SET);
  return fread(buffer, 1, count, file->fileDescriptor);
}

bool ReadLine(struct File * file, char * line, size_t maxLength)
{
  if (!file || !file->fileDescriptor)
  {
    return false;
  }
  ssize_t lineLength = getline(&line, &maxLength, file->fileDescriptor);
  if (lineLength > 0)
  {
    // Strip newline character
    line[lineLength - 1] = '\0'; 
  }
  return lineLength >= 0;
}

size_t GetFileSize(struct File * file)
{
  if (!file || !file->fileDescriptor)
  {
    return 0;
  }
  long int currentLocation = ftell(file->fileDescriptor);
  fseek(file->fileDescriptor, 0, SEEK_END);
  long int fileSize = ftell(file->fileDescriptor);
  fseek(file->fileDescriptor, currentLocation, SEEK_SET);
  return fileSize;
}

void CloseFile(struct File * file)
{
  if (file)
  {
    fclose(file->fileDescriptor);
    free(file);
  }
}

unsigned char * MapMemory(struct File * file, size_t offset, size_t length)
{
  int fileNo = fileno(file->fileDescriptor);
  void * map = mmap(NULL, length, PROT_READ, MAP_SHARED, fileNo, offset);
  if (map == MAP_FAILED)
  {
     printf("Map memory failed with %s\n", strerror(errno));
     return NULL;
  }
  return (unsigned char *)map;
}

int UnmapMemory(unsigned char * address, size_t length)
{
  return munmap((void *)address, length);
}
