#ifndef STREAMSOURCE_H
#define STREAMSOURCE_H

#include <Sensekit.h>
#include <string>
#include "Stream.h"

namespace sensekit {

    class StreamSource
    {
    public:
        StreamSource(std::string shortName)
            : m_activeStreams(0),
              m_shortName(shortName),
              m_initialized(false) {}

        virtual ~StreamSource() {}

        void initialize()
            {
                if (m_initialized)
                    return;

                on_initialized();

                m_initialized = true;
            }

        void terminate()
            {
                if (!m_initialized)
                    return;

                on_terminated();

                m_initialized = false;
            }

        virtual Stream* create_stream() = 0;
        virtual void destroy_stream(Stream* stream) = 0;

        const std::string& get_shortName() const { return m_shortName; }

    protected:
        virtual void on_initialized() {};
        virtual void on_terminated() {};

        bool m_initialized;
        unsigned m_activeStreams;
        std::string m_shortName;
    };
}

#endif /* STREAMSOURCE_H */
