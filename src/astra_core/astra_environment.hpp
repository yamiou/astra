#ifndef ASTRA_ENVIRONMENT_H
#define ASTRA_ENVIRONMENT_H

#include "astra_platform.hpp"
#include <string>

namespace astra { namespace environment {

    std::string lib_path();

#if (ASTRA_PLATFORM == ASTRA_PLATFORM_ANDROID)
    std::string application_name();
    std::string application_path();
#endif

}}

#endif /* ASTRA_ENVIRONMENT_H */
