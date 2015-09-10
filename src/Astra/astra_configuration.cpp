#include "astra_configuration.hpp"
#include "vendor/cpptoml.h"
#include <iostream>

namespace astra {

    astra_log_severity_t convert_string_to_severity(std::string s)
    {
        if (s.find("trace") != std::string::npos)
        {
            return ASTRA_SEVERITY_TRACE;
        }
        else if (s.find("warn") != std::string::npos)
        {
            return ASTRA_SEVERITY_WARN;
        }
        else if (s.find("debug") != std::string::npos)
        {
            return ASTRA_SEVERITY_DEBUG;
        }
        else if (s.find("fatal") != std::string::npos)
        {
            return ASTRA_SEVERITY_FATAL;
        }
        else if (s.find("info") != std::string::npos)
        {
            return ASTRA_SEVERITY_INFO;
        }
        else if (s.find("error") != std::string::npos)
        {
            return ASTRA_SEVERITY_ERROR;
        }

        return ASTRA_SEVERITY_UNKNOWN;
    }

    configuration::configuration()
        : pluginsPath_("Plugins/")
    {}

    configuration* configuration::load_from_file(const char* tomlFilePath)
    {
        configuration* config = new configuration();

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
            auto logLevel = t.get_qualified(loggingLevelKey)->as<std::string>()->get();
            auto severity = convert_string_to_severity(logLevel);

            if (severity != ASTRA_SEVERITY_UNKNOWN)
            {
                config->set_severityLevel(severity);
            }
        }

        const char* pluginsPathKey = "plugins.path";
        if (t.contains_qualified(pluginsPathKey))
        {
            auto pluginsPath = t.get_qualified(pluginsPathKey)->as<std::string>()->get();

            if (pluginsPath.length() > 0)
            {
                config->set_pluginsPath(pluginsPath);
            }
        }

        return config;
    }
}
