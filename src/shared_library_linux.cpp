#ifndef _WIN32

#include <dlfcn.h>
#include "shared_library.h"

void os_load_library(const char* fileName, LibHandle& libHandle)
{
    libHandle = dlopen(fileName, RTLD_NOW);

    if (libHandle == nullptr)
    {
        // error
        return;
    }
}

void os_free_library(const LibHandle libHandle)
{
    if (dlclose(libHandle) != 0)
    {
        //error
        return;
    }
}

void os_get_proc_address(const LibHandle libHandle, const char* procName, FarProc& procAddr)
{
    // Get the requested procedure address from the shared library via the OS
    procAddr = (FarProc)dlsym(libHandle, procName);
}

#endif