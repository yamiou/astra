#ifndef SHARED_LIBRARY_H
#define SHARED_LIBRARY_H

using LibHandle = void*;
using FarProc = void(*)(void*);

#define SK_STRINGIFY(n) SK_STRINGIFY_HELPER(n)
#define SK_STRINGIFY_HELPER(n) #n

void os_load_library(const char* fileName, LibHandle& libHandle);
void os_free_library(const LibHandle libHandle);
void os_get_proc_address(const LibHandle libHandle, const char* procName, FarProc& procAddr);


#endif /* SHARED_LIBRARY_H */
