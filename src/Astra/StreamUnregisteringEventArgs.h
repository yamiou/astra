#ifndef STREAMUNREGISTERINGEVENTARGS_H
#define STREAMUNREGISTERINGEVENTARGS_H

#include <Astra/astra_types.h>

namespace astra {

    class StreamSet;
    class Stream;

    struct StreamUnregisteringEventArgs
    {
        StreamSet* streamSet;
        Stream* stream;
        astra_stream_desc_t description;

        StreamUnregisteringEventArgs(StreamSet* streamSet, Stream* stream, astra_stream_desc_t description)
            : streamSet(streamSet), stream(stream), description(description)
        { }
    };

    using StreamUnregisteringCallback = std::function<void(StreamUnregisteringEventArgs)>;
}

#endif /* STREAMUNREGISTERINGEVENTARGS_H */
