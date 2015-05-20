#ifndef LOGGER_H
#define LOGGER_H

#include <SenseKit/sensekit_types.h>
#include "Logging.h"
#include <string>

namespace sensekit {

    class Logger
    {
    public:
        Logger(const char* loggerName);
        Logger();

        void log(sensekit_log_severity_t logLevel, const char* format, ...);
        void log_vargs(sensekit_log_severity_t logLevel, const char* format, va_list args);
        void trace(const char* format, ...);
        void info(const char* format, ...);
        void debug(const char* format, ...);
        void warn(const char* format, ...);
        void error(const char* format, ...);
        void fatal(const char* format, ...);

    private:
        std::string m_loggerName;
    };

    void log(const char* channel, sensekit_log_severity_t logLevel, const char* format, ...);
    void log_vargs(const char* channel, sensekit_log_severity_t logLevel, const char* format, va_list args);
    void trace(const char* channel, const char* format, ...);
    void info(const char* channel, const char* format, ...);
    void debug(const char* channel, const char* format, ...);
    void warn(const char* channel, const char* format, ...);
    void error(const char* channel, const char* format, ...);
    void fatal(const char* channel, const char* format, ...);

}

#endif /* LOGGER_H */
