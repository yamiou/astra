#ifdef _WIN32

#include "shared_library.h"

#include <Windows.h>

void os_load_library(const char* fileName, LibHandle& libHandle)
{
    char strFileName[ASTRA_FILE_MAX_PATH];
    GetFullPathName(fileName, ASTRA_FILE_MAX_PATH, strFileName, nullptr);
    libHandle = LoadLibraryEx(strFileName, nullptr, LOAD_WITH_ALTERED_SEARCH_PATH);

    if (libHandle == nullptr)
    {
        // error
        //use GetLastError()
        return;
    }
}

void os_free_library(const LibHandle libHandle)
{
    if (FreeLibrary((HMODULE)libHandle) != 0)
    {
        //error
        //use GetLastError()
        return;
    }
}

void os_get_proc_address(const LibHandle libHandle, const char* procName, FarProc& procAddr)
{
    // Get the requested procedure address from the shared library via the OS
    procAddr = (FarProc)GetProcAddress((HMODULE)libHandle, procName);
    if (procAddr == nullptr)
    {
        //error
        //use GetLastError()
    }
}

#endif