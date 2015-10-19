#ifndef ASTRA_FILESYSTEM_H
#define ASTRA_FILESYSTEM_H

#include <string>
#include "astra_platform.hpp"

#if (ASTRA_PLATFORM == ASTRA_PLATFORM_WIN32)
#include "win32/astra_filesystem_win32.hpp"
#elif defined(ASTRA_UNIXISH)
#include "unix/astra_filesystem_unix.hpp"
#else
#error "Unsupported platform!"
#endif

namespace astra { namespace filesystem {

    bool ends_with_path_separator(const std::string& path);
    std::string trim_path_separator(const std::string& path);
    std::string append_path_separator(const std::string& path);
    std::string directory_name(const std::string& filePath);
    std::string combine_paths(const std::string& left, const std::string& right);
}}

#endif /* ASTRA_FILESYSTEM_H */
