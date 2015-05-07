#ifndef LOGGER_H
#define LOGGER_H

#include <SenseKit/sensekit_types.h>
#include "vendor/easylogging++.h"
#include <cstdarg>
#include <memory>
#include <cstdio>

namespace sensekit {

    class Logger
    {
    public:
        Logger(const char* loggerName)
            : m_loggerName(loggerName)
        {
            el::Loggers::getLogger(loggerName);
            el::Loggers::addFlag(el::LoggingFlag::HierarchicalLogging);
            el::Loggers::setLoggingLevel(el::Level::Fatal);
        }

        Logger()
            : Logger("default")
        {}

        void log(sensekit_log_severity_t logLevel, const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log(logLevel, format, args);
            va_end(args);
        }

        void log_vargs(sensekit_log_severity_t logLevel, const char* format, va_list args)
        {

#ifdef _WIN32
            int len = _vscprintf(format, args);
#else
            va_list argsCopy;
            va_copy(argsCopy, args);
            int len = vsnprintf(nullptr, 0, format, argsCopy);
            va_end(argsCopy);
#endif
            std::unique_ptr<char[]> buffer(new char[len + 1]);
            vsnprintf(buffer.get(), len + 1, format, args);

            log_internal(logLevel, buffer.get());
        }

        inline void trace(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::TRACE, format, args);
            va_end(args);
        }

        inline void info(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::INFO, format, args);
            va_end(args);
        }

        inline void debug(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::DEBUG, format, args);
            va_end(args);
        }

        inline void warn(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::WARN, format, args);
            va_end(args);
        }

        inline void error(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::ERROR, format, args);
            va_end(args);
        }

        inline void fatal(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            log_vargs(sensekit_log_severity_t::FATAL, format, args);
            va_end(args);
        }

    private:
        void log_internal(sensekit_log_severity_t logLevel, const char* message)
        {
            switch(logLevel)
            {
            case TRACE:
                CLOG(TRACE, m_loggerName.c_str()) << message;
                break;
            case DEBUG:
                CLOG(DEBUG, m_loggerName.c_str()) << message;
                break;
            case INFO:
                CLOG(INFO, m_loggerName.c_str()) << message;
                break;
            case WARN:
                CLOG(WARNING, m_loggerName.c_str()) << message;
                break;
            case ERROR:
                CLOG(ERROR, m_loggerName.c_str()) << message;
                break;
            case FATAL:
                CLOG(FATAL, m_loggerName.c_str()) << message;
                break;
            }
        }

        std::string m_loggerName;
    };
}

#endif /* LOGGER_H */
