#ifndef PLUGINLOGGER_H
#define PLUGINLOGGER_H

#include <Astra/Plugins/PluginServiceProxy.h>
#include <cstdarg>

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

#define STRACE(channel, format, ...) astra::plugins::log(channel, SK_TRACE, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)


#define SINFO(channel, format, ...) astra::plugins::log(channel, SK_INFO, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)


#define SDEBUG(channel, format, ...) astra::plugins::log(channel, SK_DEBUG, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)


#define SERROR(channel, format, ...) astra::plugins::log(channel, SK_ERROR, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)

#define SFATAL(channel, format, ...) astra::plugins::log(channel, SK_FATAL, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)


#define SWARN(channel, format, ...) astra::plugins::log(channel, SK_WARN, __FILE__, __LINE__, LOG_FUNC, format, ##__VA_ARGS__)

extern astra::PluginServiceProxy* __g_serviceProxy;

namespace astra { namespace plugins {

    inline void log(const char* channel,
                    astra_log_severity_t logLevel,
                    const char* fileName,
                    int lineNo,
                    const char* func,
                    const char* format,
                    ...)
    {
        va_list args;
        va_start(args, format);
        __g_serviceProxy->log(channel, logLevel, fileName, lineNo, func, format, args);
        va_end(args);
    }
}}
#endif /* PLUGINLOGGER_H */
