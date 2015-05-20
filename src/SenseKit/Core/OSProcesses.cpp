#include "OSProcesses.h"

namespace sensekit {

#if __ANDROID__
    std::string get_application_name()
    {
        FILE *file;
        size_t length;

        const size_t NAME_LENGTH = 1024;
        char appName[NAME_LENGTH];

        file = fopen("/proc/self/cmdline", "r");
        length = fread(appName, 1, NAME_LENGTH, file);
        fclose(file);

        return std::string(appName);
    }

    std::string get_application_filepath()
    {
        const size_t PATH_LENGTH = 1024;
        char path[PATH_LENGTH];

        std::string appName = get_application_name();
        snprintf(path, PATH_LENGTH, "/data/data/%s/files/", appName.c_str());

        return std::string(path);
    }

    std::string get_applictaion_libpath()
    {
        const size_t PATH_LENGTH = 1024;
        char path[PATH_LENGTH];

        std::string appName = get_application_name();
        snprintf(path, PATH_LENGTH, "/data/data/%s/lib/", appName.c_str());

        return std::string(path);
    }
#endif

}
