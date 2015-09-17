#include "astra_string.hpp"

namespace astra {

    std::string trim_start(const std::string& str, char trimChar)
    {
        if (str.length() == 0)
            return str;

        auto it = str.cbegin();
        while(it++ != str.cbegin() && *it == trimChar) {}

        return std::string(it - 1, str.cend());
    }

    std::string trim_end(const std::string& str, char trimChar)
    {
        if (str.length() == 0)
            return str;

        auto it = str.cend();
        while(--it != str.cbegin() && *it == trimChar) {}

        return std::string(str.cbegin(), it + 1);
    }
}
