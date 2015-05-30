#include "Logger.h"
#include "Logging.h"
#include <cstdarg>
#include <memory>
#include <cstdio>

namespace sensekit {

    void log_nyan(Logger& logger)
    {
        logger.info("+      o     +              o");
        logger.info("    +             o      +       +");
        logger.info("o          +");
        logger.info("    o  +           +        +");
        logger.info("+        o     o       +        o");
        logger.info("-_-_-_-_-_-_-_,------,      o");
        logger.info("_-_-_-_-_-_-_-|   /\\_/\\");
        logger.info("-_-_-_-_-_-_-~|__( ^ .^)  +     +");
        logger.info("_-_-_-_-_-_-_-\"\"  \"\"");
        logger.info("+      o         o   +       o");
        logger.info("    +         +");
        logger.info("o        o         o      o     +");
        logger.info("    o           +");
        logger.info("+      +     o        o      +");
    }

    static void dispatch_log(const char* channel, sensekit_log_severity_t logLevel, const char* message)
    {
        switch(logLevel)
        {
        case SK_TRACE:
            CLOG(TRACE, channel) << message;
            break;
        case SK_DEBUG:
            CLOG(DEBUG, channel) << message;
            break;
        case SK_INFO:
            CLOG(INFO, channel) << message;
            break;
        case SK_WARN:
            CLOG(WARNING, channel) << message;
            break;
        case SK_ERROR:
            CLOG(ERROR, channel) << message;
            break;
        case SK_FATAL:
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
        log_vargs(channel, sensekit_log_severity_t::SK_TRACE, format, args);
        va_end(args);
    }

    void info(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::SK_INFO, format, args);
        va_end(args);
    }

    void debug(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::SK_DEBUG, format, args);
        va_end(args);
    }

    void warn(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::SK_WARN, format, args);
        va_end(args);
    }

    void error(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::SK_ERROR, format, args);
        va_end(args);
    }

    void fatal(const char* channel, const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, sensekit_log_severity_t::SK_FATAL, format, args);
        va_end(args);
    }

    void Logger::trace(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_TRACE, format, args);
        va_end(args);
    }

    void Logger::info(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_INFO, format, args);
        va_end(args);
    }

    void Logger::debug(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_DEBUG, format, args);
        va_end(args);
    }

    void Logger::warn(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_WARN, format, args);
        va_end(args);
    }

    void Logger::error(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_ERROR, format, args);
        va_end(args);
    }

    void Logger::fatal(const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(sensekit_log_severity_t::SK_FATAL, format, args);
        va_end(args);
    }
}
