#ifndef PLUGINLOGGER_H
#define PLUGINLOGGER_H

#include <SenseKit/Plugins/PluginServiceProxy.h>

namespace sensekit { namespace plugins {

    class PluginLogger
    {
    public:
        PluginLogger(PluginServiceProxy& pluginService)
            : m_pluginService(pluginService)
        {}

        inline void log(sensekit_log_severity_t logLevel, const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(logLevel, format, args);
            va_end(args);
        }

        inline void trace(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::TRACE, format, args);
            va_end(args);
        }

        inline void info(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::INFO, format, args);
            va_end(args);
        }

        inline void debug(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::DEBUG, format, args);
            va_end(args);
        }

        inline void warn(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::WARN, format, args);
            va_end(args);
        }

        inline void error(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::ERROR, format, args);
            va_end(args);
        }

        inline void fatal(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(sensekit_log_severity_t::FATAL, format, args);
            va_end(args);
        }
    private:
        PluginServiceProxy& m_pluginService;
    };


}}


#endif /* PLUGINLOGGER_H */
