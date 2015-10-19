#include "../astra_filesystem.hpp"

#include <string>

namespace astra { namespace filesystem {

    std::string directory_name(const std::string& filePath)
    {
        const size_t PATH_LENGTH = 1024;
        char fullPath[PATH_LENGTH];
        char* fileNameBegin;

        DWORD result = ::GetFullPathName(filePath.c_str(), PATH_LENGTH, fullPath, &fileNameBegin);

        if (!result)
        {
            return std::string();
        }

        fileNameBegin[0] = '\0';
        std::string directoryName(trim_path_separator(fullPath));

        return directoryName;
    }
}}
