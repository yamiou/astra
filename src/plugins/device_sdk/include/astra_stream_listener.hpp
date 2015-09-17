#ifndef ASTRA_STREAM_LISTENER_H
#define ASTRA_STREAM_LISTENER_H

#include "astra_sensor_stream.hpp"
#include <memory>

namespace astra { namespace devices {

    class stream_listener
    {
    public:
        virtual ~stream_listener() {}

        virtual void state_changed(sensor_stream::shared_ptr stream) {}
        virtual void property_changed(sensor_stream::shared_ptr stream, sensor_stream::property_id id) {}
        virtual void streaming_changed(sensor_stream::shared_ptr stream) {}
        virtual void new_frame_available(sensor_stream::shared_ptr stream) {}
    };
}}

#endif /* ASTRA_STREAM_LISTENER_H */
