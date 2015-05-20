#ifndef LOGGING_H
#define LOGGING_H

#ifndef __ANDROID__
#define ELPP_STACKTRACE_ON_CRASH
#endif

#define ELPP_NO_DEFAULT_LOG_FILE

#include "vendor/easylogging++.h"

#define INITIALIZE_LOGGING INITIALIZE_EASYLOGGINGPP

namespace sensekit {
    void initialize_logging(const char* logFilePath);
}

#endif /* LOGGING_H */
