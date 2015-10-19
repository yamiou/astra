#include "astra_filesystem.hpp"
#include "astra_string.hpp"

#include <string>

namespace astra { namespace filesystem {

    bool ends_with_path_separator(const std::string& path)
    {
        return path.length() > 0 && path[path.length() - 1] == filesystem::path_separator;
    }

    std::string trim_path_separator(const std::string& path)
    {
        return trim_end(path, filesystem::path_separator);
    }

    std::string append_path_separator(const std::string& path)
    {
        return trim_path_separator(path) + filesystem::path_separator;
    }

    std::string combine_paths(const std::string& left, const std::string& right)
    {
        return trim_path_separator(left) + filesystem::path_separator + right;
    }
}}
