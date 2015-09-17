#include "astra_filesystem_unix.hpp"
#include <string>
#include <libgen.h>

namespace astra { namespace filesystem {

    std::string directory_name(const std::string& filePath)
    {
        const std::size_t PATH_LENGTH = 1024;
        char pathCopy[PATH_LENGTH];

        // copy because dirname operates in-place
        std::copy(filePath.cbegin(), filePath.cend(), pathCopy);
        pathCopy[filePath.length()] = '\0';

        return ::dirname(pathCopy);
    }

}}
