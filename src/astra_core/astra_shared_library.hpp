#ifndef ASTRA_SHARED_LIBRARY_H
#define ASTRA_SHARED_LIBRARY_H

#ifdef _WIN32
#define ASTRA_FILE_MAX_PATH MAX_PATH
#else
#define ASTRA_FILE_MAX_PATH 256
#endif

#define ASTRA_STRINGIFY(n) ASTRA_STRINGIFY_HELPER(n)
#define ASTRA_STRINGIFY_HELPER(n) #n

namespace astra { namespace process {

    using lib_handle = void*;
    using far_proc = void(*)(void*);

    void load_library(const char* fileName, lib_handle& libHandle);
    void free_library(const lib_handle libHandle);
    void get_proc_address(const lib_handle libHandle, const char* procName, far_proc& procAddr);

}}

#endif /* ASTRA_SHARED_LIBRARY_H */
