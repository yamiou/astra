#ifndef ASTRA_STREAM_LISTENER_H
#define ASTRA_STREAM_LISTENER_H

#include "astra_stream.hpp"

namespace astra {

    class stream_listener
    {
    public:
        virtual ~stream_listener() {}
        virtual void on_stream_registered(stream* stream) {}
        virtual void on_stream_unregistering(stream* stream) {}
    };
}

#endif /* ASTRA_STREAM_LISTENER_H */
