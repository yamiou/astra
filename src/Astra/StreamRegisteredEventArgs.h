#ifndef STREAMREGISTEREDEVENTARGS_H
#define STREAMREGISTEREDEVENTARGS_H

#include <Astra/astra_types.h>

namespace astra {

    class StreamSet;
    class Stream;

    struct StreamRegisteredEventArgs
    {
        StreamSet* streamSet;
        Stream* stream;
        astra_stream_desc_t description;

        StreamRegisteredEventArgs(StreamSet* streamSet, Stream* stream, astra_stream_desc_t description)
            : streamSet(streamSet), stream(stream), description(description)
        { }
    };

    using StreamRegisteredCallback = std::function<void(StreamRegisteredEventArgs)>;
}

#endif /* STREAMREGISTEREDEVENTARGS_H */
