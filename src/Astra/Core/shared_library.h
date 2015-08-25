#ifndef SHARED_LIBRARY_H
#define SHARED_LIBRARY_H

using LibHandle = void*;
using FarProc = void(*)(void*);

#define ASTRA_STRINGIFY(n) ASTRA_STRINGIFY_HELPER(n)
#define ASTRA_STRINGIFY_HELPER(n) #n

#ifdef _WIN32
    #define ASTRA_FILE_MAX_PATH MAX_PATH
#else
    #define ASTRA_FILE_MAX_PATH 256
#endif

void os_load_library(const char* fileName, LibHandle& libHandle);
void os_free_library(const LibHandle libHandle);
void os_get_proc_address(const LibHandle libHandle, const char* procName, FarProc& procAddr);

#endif /* SHARED_LIBRARY_H */
