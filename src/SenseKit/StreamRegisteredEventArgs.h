#ifndef STREAMREGISTEREDEVENTARGS_H
#define STREAMREGISTEREDEVENTARGS_H

#include <SenseKit/sensekit_types.h>

namespace sensekit {

    class StreamSet;
    class Stream;

    struct StreamRegisteredEventArgs
    {
        StreamSet* streamSet;
        Stream* stream;
        sensekit_stream_desc_t description;

        StreamRegisteredEventArgs(StreamSet* streamSet, Stream* stream, sensekit_stream_desc_t description)
            : streamSet(streamSet), stream(stream), description(description)
        { }
    };

    using StreamRegisteredCallback = std::function<void(StreamRegisteredEventArgs)>;
}

#endif /* STREAMREGISTEREDEVENTARGS_H */
