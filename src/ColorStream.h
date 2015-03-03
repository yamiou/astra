#ifndef COLORSTREAM_H
#define COLORSTREAM_H

#include "Stream.h"

namespace sensekit {

    class ColorStream : public Stream
    {
    public:
        ColorStream(StreamSource& source)
            : Stream(source) { }

        virtual ~ColorStream() { }
    };

}

#endif /* COLORSTREAM_H */
