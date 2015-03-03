#ifndef STREAMSOURCE_H
#define STREAMSOURCE_H

#include "Stream.h"

namespace sensekit {

    class StreamSource
    {
    public:
        StreamSource() {};
        virtual ~StreamSource() {};

        virtual Stream* create_stream() = 0;
        virtual void destroy_stream(Stream* stream) = 0;
    };

}

#endif /* STREAMSOURCE_H */
