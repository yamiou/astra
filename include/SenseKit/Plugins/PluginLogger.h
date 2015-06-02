#ifndef PLUGINLOGGER_H
#define PLUGINLOGGER_H

#include <SenseKit/Plugins/PluginServiceProxy.h>
#include <string>

namespace sensekit { namespace plugins {

    class PluginLogger
    {
    public:
        PluginLogger(PluginServiceProxy& pluginService, const char* loggerName)
            : m_pluginService(pluginService),
              m_loggerName(loggerName)
        {}

        inline void log(sensekit_log_severity_t logLevel, const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), logLevel, format, args);
            va_end(args);
        }

        inline void trace(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_TRACE, format, args);
            va_end(args);
        }

        inline void info(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_INFO, format, args);
            va_end(args);
        }

        inline void debug(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_DEBUG, format, args);
            va_end(args);
        }

        inline void warn(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_WARN, format, args);
            va_end(args);
        }

        inline void error(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_ERROR, format, args);
            va_end(args);
        }

        inline void fatal(const char* format, ...)
        {
            va_list args;
            va_start(args, format);
            m_pluginService.log(m_loggerName.c_str(), sensekit_log_severity_t::SK_FATAL, format, args);
            va_end(args);
        }
    private:
        PluginServiceProxy& m_pluginService;
        std::string m_loggerName;
    };

}}


#endif /* PLUGINLOGGER_H */
