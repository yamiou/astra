#ifndef ASTRA_STREAM_REGISTERED_EVENT_ARGS_H
#define ASTRA_STREAM_REGISTERED_EVENT_ARGS_H

#include <astra_core/capi/astra_types.h>

namespace astra {

    class streamset;
    class stream;

    struct stream_registered_event_args
    {
        streamset* streamSet;
        astra::stream* stream;
        astra_stream_desc_t description;

        stream_registered_event_args(streamset* streamSet, astra::stream* strm, astra_stream_desc_t description)
            : streamSet(streamSet), stream(strm), description(description)
        { }
    };

    using StreamRegisteredCallback = std::function<void(stream_registered_event_args)>;
}

#endif /* ASTRA_STREAM_REGISTERED_EVENT_ARGS_H */
