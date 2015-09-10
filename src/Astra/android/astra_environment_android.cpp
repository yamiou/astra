#include "OSProcesses.h"

namespace astra { namespace environment {

    std::string application_name()
    {
        FILE *file;

        const size_t NAME_LENGTH = 1024;
        char appName[NAME_LENGTH];

        file = fopen("/proc/self/cmdline", "r");
        fread(appName, 1, NAME_LENGTH, file);
        fclose(file);

        return std::string(appName);
    }

    std::string application_path()
    {
        const size_t PATH_LENGTH = 1024;
        char path[PATH_LENGTH];

        std::string appName = get_application_name();
        snprintf(path, PATH_LENGTH, "/data/data/%s/files/", appName.c_str());

        return std::string(path);
    }

    std::string libpath()
    {
        const size_t PATH_LENGTH = 1024;
        char path[PATH_LENGTH];

        std::string appName = get_application_name();
        snprintf(path, PATH_LENGTH, "/data/data/%s/lib/", appName.c_str());

        return std::string(path);
    }

}}
