#ifndef LOGGING_H
#define LOGGING_H

#include <SenseKit/sensekit_types.h>

#ifndef __ANDROID__
#define ELPP_STACKTRACE_ON_CRASH
#endif

#define ELPP_NO_DEFAULT_LOG_FILE
#include "vendor/easylogging++.h"

#define INITIALIZE_LOGGING INITIALIZE_EASYLOGGINGPP

#endif /* LOGGING_H */
