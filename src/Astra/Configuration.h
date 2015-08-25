#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <Astra/astra_types.h>

namespace astra {

    class Configuration
    {
    public:
        astra_log_severity_t severityLevel() { return m_severityLevel; }
        void set_severityLevel(astra_log_severity_t level) { m_severityLevel = level; }

        static Configuration* load_from_file(const char* tomlFilePath);

    private:
        astra_log_severity_t m_severityLevel{ASTRA_SEVERITY_INFO};
    };
}

#endif /* CONFIGURATION_H */
