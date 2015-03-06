#ifndef STREAMSOURCE_H
#define STREAMSOURCE_H

#include <Sensekit.h>
#include "Stream.h"

namespace sensekit {

    class StreamSource
    {
    public:
        StreamSource(sensekit_streamsource_desc_t desc)
            : m_activeStreams(0),
              m_sourceDesc(desc)
            { }

        virtual ~StreamSource() {};

        virtual Stream* create_stream() = 0;

        virtual void destroy_stream(Stream* stream) = 0;

        virtual void open_stream(Stream* stream) = 0;

        const sensekit_streamsource_desc_t& get_description() { return m_sourceDesc; }

    protected:
        unsigned m_activeStreams;
        sensekit_streamsource_desc_t m_sourceDesc;

    };

}

#endif /* STREAMSOURCE_H */
