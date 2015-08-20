#include "Logging.h"

#include <Astra/astra_types.h>

namespace astra {

    el::Level convert_sk_to_elpp_level(astra_log_severity_t severity)
    {
        switch (severity)
        {
        case SK_INFO:
            return el::Level::Info;
        case SK_WARN:
            return el::Level::Warning;
        case SK_ERROR:
            return el::Level::Error;
        case SK_FATAL:
            return el::Level::Fatal;
        case SK_DEBUG:
            return el::Level::Debug;
        case SK_TRACE:
            return el::Level::Trace;
        default:
            return el::Level::Unknown;
        }
    }

    void initialize_logging(const char* logFilePath, astra_log_severity_t severity)
    {
        const char TRUE_STRING[] = "true";

        el::Loggers::addFlag(el::LoggingFlag::CreateLoggerAutomatically);
        el::Loggers::addFlag(el::LoggingFlag::ColoredTerminalOutput);
        el::Loggers::addFlag(el::LoggingFlag::HierarchicalLogging);

        el::Configurations defaultConf;

        defaultConf.setToDefault();
        defaultConf.setGlobally(el::ConfigurationType::Enabled, TRUE_STRING);
        defaultConf.setGlobally(el::ConfigurationType::ToStandardOutput, TRUE_STRING);
        defaultConf.setGlobally(el::ConfigurationType::ToFile, TRUE_STRING);
        defaultConf.setGlobally(el::ConfigurationType::Filename, logFilePath);

        el::Loggers::setDefaultConfigurations(defaultConf, true);
        el::Loggers::setLoggingLevel(convert_sk_to_elpp_level(severity));

        defaultConf.clear();
    }
}
