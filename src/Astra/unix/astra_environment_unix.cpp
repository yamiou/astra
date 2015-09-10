#include <string>
#include <dlfcn.h>
#include <libgen.h>
#include "../astra_filesystem.hpp"

namespace astra { namespace environment {

    std::string module_path_for_proc_address(void* procAddress)
    {
        Dl_info info;
        if (!dladdr(procAddress, &info))
        {
            return std::string();
        }

        return std::string(info.dli_fname);
    }

    std::string lib_path()
    {
        return astra::filesystem::directory_name(module_path_for_proc_address(reinterpret_cast<void*>(&lib_path)));
    }
}}
