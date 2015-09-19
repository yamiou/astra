#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#include "../astra_shared_library.hpp"

namespace astra { namespace process {

    void load_library(const char* fileName, lib_handle& libHandle)
    {
        libHandle = ::dlopen(fileName, RTLD_NOW);

        if (libHandle == nullptr)
        {
            // error
            return;
        }
    }

    void free_library(const lib_handle libHandle)
    {
        if (::dlclose(libHandle) != 0)
        {
            //error
            return;
        }
    }

    void get_proc_address(const lib_handle libHandle, const char* procName, far_proc& procAddr)
    {
        // Get the requested procedure address from the shared library via the OS
        procAddr = (far_proc)::dlsym(libHandle, procName);
    }

}}
