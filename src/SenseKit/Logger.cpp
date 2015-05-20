#include "Logger.h"
#include "Logging.h"
#include <cstdarg>
#include <memory>
#include <cstdio>

namespace sensekit {

    static void dispatch_log(const char* channel, sensekit_log_severity_t logLevel, const char* message)
    {
        switch(logLevel)
        {
        case TRACE:
            CLOG(TRACE, channel) << message;
            break;
        case DEBUG:
            CLOG(DEBUG, channel) << message;
            break;
        case INFO:
            CLOG(INFO, channel) << message;
            break;
        case WARN:
            CLOG(WARNING, channel) << message;
            break;
        case ERROR:
            CLOG(ERROR, channel) << message;
            break;
        case FATAL:
            CLOG(FATAL, channel) << message;
            break;
        }
    }

    Logger::Logger(const char* loggerName)
        : m_loggerName(loggerName)
    {
        el::Loggers::getLogger(loggerName);
    }

    Logger::Logger()
        : Logger("default")
    {}

    void Logger::log(sensekit_log_severity_t logLevel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(logLevel, format, args);
        va_end(args);
    }


    void log_vargs(const char* channel, sensekit_log_severity_t logLevel, const char* format, va_list args)
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

        dispatch_log(channel, logLevel, buffer.get());
    }

    void log(const char* channel, sensekit_log_severity_t logLevel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, logLevel, format, args);
        va_end(args);
    }

    void Logger::log_vargs(sensekit_log_severity_t logLevel, const char* format, va_list args)
    {
        sensekit::log_vargs(m_loggerName.c_str(), logLevel, format, args);
    }

    void trace(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::TRACE, format, args);
        va_end(args);
    }

    void info(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::INFO, format, args);
        va_end(args);
    }

    void debug(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::DEBUG, format, args);
        va_end(args);
    }

    void warn(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::WARN, format, args);
        va_end(args);
    }

    void error(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::ERROR, format, args);
        va_end(args);
    }

    void fatal(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::FATAL, format, args);
        va_end(args);
    }

    void Logger::trace(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::TRACE, format, args);
        va_end(args);
    }

    void Logger::info(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::INFO, format, args);
        va_end(args);
    }

    void Logger::debug(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::DEBUG, format, args);
        va_end(args);
    }

    void Logger::warn(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::WARN, format, args);
        va_end(args);
    }

    void Logger::error(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::ERROR, format, args);
        va_end(args);
    }

    void Logger::fatal(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::FATAL, format, args);
        va_end(args);
    }
}
