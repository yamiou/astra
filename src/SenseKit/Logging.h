#ifndef LOGGING_H
#define LOGGING_H

#ifdef __ANDROID__
#define ELPP_DISABLE_DEFAULT_CRASH_HANDLING
#else // not android
#define ELPP_STACKTRACE_ON_CRASH
#endif

#define ELPP_NO_DEFAULT_LOG_FILE


#include "vendor/easylogging++.h"
#include <SenseKit/sensekit_types.h>

#define INITIALIZE_LOGGING INITIALIZE_EASYLOGGINGPP

namespace sensekit {
    void initialize_logging(const char* logFilePath, sensekit_log_severity_t severity);
}

#endif /* LOGGING_H */
