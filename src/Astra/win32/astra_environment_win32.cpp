#include "../astra_filesystem.hpp"

#include <windows.h>
#include <string>

namespace astra { namespace environment {

    namespace detail {

        std::string module_path_for_proc_address(void* procAddress)
        {
            HMODULE hModule;
            BOOL rc = ::GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                                          (LPCSTR)procAddress,
                                          &hModule);

            if (!rc)
            {
                return std::string();
            }

            std::string path;
            const size_t PATH_LENGTH = 1024;
            path.resize(PATH_LENGTH);

            DWORD len = ::GetModuleFileName(hModule, &path[0], PATH_LENGTH);

            if (len == 0)
            {
                return std::string();
            }

            path.resize(len);

            return path;
        }
    }

    std::string lib_path()
    {
        return filesystem::directory_name(detail::module_path_for_proc_address(reinterpret_cast<void*>(&lib_path)));
    }
}}
