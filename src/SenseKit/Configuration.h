#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <SenseKit/sensekit_types.h>

namespace sensekit {

    class Configuration
    {
    public:
        sensekit_log_severity_t severityLevel() { return m_severityLevel; }
        void set_severityLevel(sensekit_log_severity_t level) { m_severityLevel = level; }

        static Configuration* load_from_file(const char* tomlFilePath);

    private:
        sensekit_log_severity_t m_severityLevel{SK_INFO};
    };
}

#endif /* CONFIGURATION_H */
