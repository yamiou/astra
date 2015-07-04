#ifndef LOGGER_H
#define LOGGER_H

#include <SenseKit/sensekit_types.h>
#include "Logging.h"
#include <string>

#if defined(_MSC_VER)  // Visual C++
#   define LOG_FUNC __FUNCSIG__
#elif defined(__GNUC__)  // GCC
#   define LOG_FUNC __PRETTY_FUNCTION__
#elif defined(__INTEL_COMPILER)  // Intel C++
#   define LOG_FUNC __PRETTY_FUNCTION__
#elif defined(__clang__)  // Clang++
#   define LOG_FUNC __PRETTY_FUNCTION__
#else
#   if defined(__func__)
#      define LOG_FUNC __func__
#   else
#      define LOG_FUNC ""
#   endif  // defined(__func__)
#endif  // defined(_MSC_VER)

#define STRACE(channel, format, ...) ::sensekit::log(channel, SK_TRACE, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)


#define SINFO(channel, format, ...) ::sensekit::log(channel, SK_INFO, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)


#define SDEBUG(channel, format, ...) ::sensekit::log(channel, SK_DEBUG, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)


#define SERROR(channel, format, ...) ::sensekit::log(channel, SK_ERROR, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)

#define SFATAL(channel, format, ...) ::sensekit::log(channel, SK_FATAL, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)


#define SWARN(channel, format, ...) ::sensekit::log(channel, SK_WARN, __FILE__, __LINE__, LOG_FUNC, format , ##__VA_ARGS__)

namespace sensekit {

    void log(const char* channel,
             sensekit_log_severity_t logLevel,
             const char* fileName,
             int lineNo,
             const char* func,
             const char* format, ...);


    void log_vargs(const char* channel,
                   sensekit_log_severity_t logLevel,
                   const char* fileName,
                   int lineNo,
                   const char* func,
                   const char* format,
                   va_list args);

    inline void log_nyan()
    {
        SINFO("Nyan", "+      o     +              o");
        SINFO("Nyan", "    +             o      +       +");
        SINFO("Nyan", "o          +");
        SINFO("Nyan", "    o  +           +        +");
        SINFO("Nyan", "+        o     o       +        o");
        SINFO("Nyan", "-_-_-_-_-_-_-_,------,      o");
        SINFO("Nyan", "_-_-_-_-_-_-_-|   /\\_/\\");
        SINFO("Nyan", "-_-_-_-_-_-_-~|__( ^ .^)  +     +");
        SINFO("Nyan", "_-_-_-_-_-_-_-\"\"  \"\"");
        SINFO("Nyan", "+      o         o   +       o");
        SINFO("Nyan", "    +         +");
        SINFO("Nyan", "o        o         o      o     +");
        SINFO("Nyan", "    o           +");
        SINFO("Nyan", "+      +     o        o      +");
    }
}

#endif /* LOGGER_H */
