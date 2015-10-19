#ifndef ASTRA_CONFIGURATION_H
#define ASTRA_CONFIGURATION_H

#include <astra_core/capi/astra_types.h>
#include <string>

namespace astra {

    class configuration
    {
    public:
        configuration();

        astra_log_severity_t severityLevel() { return severityLevel_; }
        void set_severityLevel(astra_log_severity_t level) { severityLevel_ = level; }

        const std::string& pluginsPath() const { return pluginsPath_; }
        void set_pluginsPath(std::string pluginsPath) { pluginsPath_ = pluginsPath; }

        static configuration* load_from_file(const char* tomlFilePath);

    private:
        astra_log_severity_t severityLevel_{ASTRA_SEVERITY_FATAL};
        std::string pluginsPath_;
    };
}

#endif /* ASTRA_CONFIGURATION_H */
