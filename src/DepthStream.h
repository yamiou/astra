#ifndef DEPTHSTREAM_H
#define DEPTHSTREAM_H

#include "Stream.h"

namespace sensekit {

    class DepthStream : public Stream
    {
    public:
        DepthStream(StreamSource& source)
            : Stream(source) { }

        virtual ~DepthStream() { }
    };

}

#endif /* DEPTHSTREAM_H */
