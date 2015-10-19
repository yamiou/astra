#ifndef ASTRA_STRING_H
#define ASTRA_STRING_H

#include <string>

namespace astra {

    std::string trim_start(const std::string& str, char trimChar);
    std::string trim_end(const std::string& str, char trimChar);
}

#endif /* ASTRA_STRING_H */
