#ifndef ASTRA_STREAM_UNREGISTERING_EVENT_ARGS_H
#define ASTRA_STREAM_UNREGISTERING_EVENT_ARGS_H

#include <Astra/astra_types.h>

namespace astra {

    class streamset;
    class stream;

    struct stream_unregistering_event_args
    {
        streamset* streamSet;
        astra::stream* stream;
        astra_stream_desc_t description;

        stream_unregistering_event_args(streamset* streamSet, astra::stream* strm, astra_stream_desc_t description)
            : streamSet(streamSet), stream(strm), description(description)
        { }
    };

    using StreamUnregisteringCallback = std::function<void(stream_unregistering_event_args)>;
}

#endif /* ASTRA_STREAM_UNREGISTERING_EVENT_ARGS_H */
