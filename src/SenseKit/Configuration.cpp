#include "Configuration.h"
#include "vendor/cpptoml.h"

namespace sensekit {

    sensekit_log_severity_t convert_string_to_severity(std::string s)
    {
        if (s.find("trace") != std::string::npos)
        {
            return SK_TRACE;
        }
        else if (s.find("warn") != std::string::npos)
        {
            return SK_WARN;
        }
        else if (s.find("debug") != std::string::npos)
        {
            return SK_DEBUG;
        }
        else if (s.find("fatal") != std::string::npos)
        {
            return SK_FATAL;
        }
        else if (s.find("info") != std::string::npos)
        {
            return SK_INFO;
        }
        else if (s.find("error") != std::string::npos)
        {
            return SK_ERROR;
        }

        return SK_UNKNOWN;
    }

    Configuration* Configuration::load_from_file(const char* tomlFilePath)
    {
        Configuration* config = new Configuration();

        cpptoml::table t;

        try
        {
            t = cpptoml::parse_file(tomlFilePath);
        }
        catch (const cpptoml::parse_exception& e)
        {
            return config;
        }

        const char* loggingLevelKey = "logging.level";
        if (t.contains_qualified(loggingLevelKey))
        {
            auto logLevel = *t.get_qualified_as<std::string>(loggingLevelKey);
            auto severity = convert_string_to_severity(logLevel);

            if (severity != SK_UNKNOWN)
            {
                config->set_severityLevel(severity);
            }
        }

        return config;
    }
}
