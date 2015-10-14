#define WIN32_LEAN_AND_MEAN

#include <Windows.h>

#include "../astra_shared_library.hpp"

namespace astra { namespace process {

    void load_library(const char* fileName, lib_handle& libHandle)
    {
        char strFileName[ASTRA_FILE_MAX_PATH];
        ::GetFullPathName(fileName, ASTRA_FILE_MAX_PATH, strFileName, nullptr);
        libHandle = LoadLibraryEx(strFileName, nullptr, LOAD_WITH_ALTERED_SEARCH_PATH);

        if (libHandle == nullptr)
        {
            // error
            //use GetLastError()
            return;
        }
    }

    void free_library(const lib_handle libHandle)
    {
        if (::FreeLibrary((HMODULE)libHandle) != 0)
        {
            //error
            //use GetLastError()
            return;
        }
    }

    void get_proc_address(const lib_handle libHandle, const char* procName, far_proc& procAddr)
    {
        // Get the requested procedure address from the shared library via the OS
        procAddr = (far_proc)::GetProcAddress((HMODULE)libHandle, procName);
        if (procAddr == nullptr)
        {
            //error
            //use GetLastError()
        }
    }

}}