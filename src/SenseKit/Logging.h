#ifndef LOGGING_H
#define LOGGING_H

#include <SenseKit/sensekit_types.h>
#include "vendor/easylogging++.h"

#define INITIALIZE_LOGGING INITIALIZE_EASYLOGGINGPP

namespace sensekit {

    inline void log(sensekit_log_severity_t logLevel, const char* message)
    {
        switch(logLevel)
        {
        case TRACE:
            LOG(TRACE) << message;
            break;
        case DEBUG:
            LOG(DEBUG) << message;
            break;
        case INFO:
            LOG(INFO) << message;
            break;
        case WARN:
            LOG(WARNING) << message;
            break;
        case ERROR:
            LOG(ERROR) << message;
            break;
        case FATAL:
            LOG(FATAL) << message;
            break;
        }
    }
}

#endif /* LOGGING_H */
