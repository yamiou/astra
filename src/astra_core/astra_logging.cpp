// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#include "astra_logging.hpp"

#include <astra_core/capi/astra_types.h>

namespace astra {

    el::Level convert_sk_to_elpp_level(astra_log_severity_t severity)
    {
        switch (severity)
        {
        case ASTRA_SEVERITY_INFO:
            return el::Level::Info;
        case ASTRA_SEVERITY_WARN:
            return el::Level::Warning;
        case ASTRA_SEVERITY_ERROR:
            return el::Level::Error;
        case ASTRA_SEVERITY_FATAL:
            return el::Level::Fatal;
        case ASTRA_SEVERITY_DEBUG:
            return el::Level::Debug;
        case ASTRA_SEVERITY_TRACE:
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
