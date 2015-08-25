#include "Logger.h"
#include "Logging.h"
#include <cstdarg>
#include <memory>
#include <cstdio>

namespace astra {

    static void dispatch_log(const char* fileName,
                             int lineNo,
                             const char* func,
                             const char* channel,
                             astra_log_severity_t logLevel,
                             const char* message)
    {
        switch(logLevel)
        {
        case ASTRA_SEVERITY_TRACE:
            el::base::Writer(el::Level::Trace,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_DEBUG:
            el::base::Writer(el::Level::Debug,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_INFO:
            el::base::Writer(el::Level::Info,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_WARN:
            el::base::Writer(el::Level::Warning,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_ERROR:
            el::base::Writer(el::Level::Error,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_FATAL:
            el::base::Writer(el::Level::Fatal,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        case ASTRA_SEVERITY_UNKNOWN:
            el::base::Writer(el::Level::Unknown,
                             fileName,
                             lineNo,
                             func,
                             el::base::DispatchAction::NormalLog)
                .construct(1, channel) << message;

            break;
        }
    }

    void log_vargs(const char* channel,
                   astra_log_severity_t logLevel,
                   const char* fileName,
                   int lineNo,
                   const char* func,
                   const char* format,
                   va_list args)
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

        dispatch_log(fileName, lineNo, func, channel, logLevel, buffer.get());
    }

    void log(const char* channel,
             astra_log_severity_t logLevel,
             const char* fileName,
             int lineNo,
             const char* func,
             const char* format,
             ...)
    {
        va_list args;
        va_start(args, format);
        log_vargs(channel, logLevel, fileName, lineNo, func, format, args);
        va_end(args);
    }
}
