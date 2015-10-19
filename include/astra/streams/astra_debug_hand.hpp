#ifndef ASTRA_HAND_DEBUG_HPP
#define ASTRA_HAND_DEBUG_HPP

#include <astra_core/astra_core.hpp>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/hand_capi.h>

namespace astra {

    using DebugHandViewType = astra_debug_hand_view_type_t;

    class debug_handstream : public datastream
    {
    public:
        explicit debug_handstream(astra_streamconnection_t connection)
            : datastream(connection),
              debugHandStream_(connection)
        { }

        static const astra_stream_type_t id = ASTRA_STREAM_DEBUG_HAND;

        void set_view_type(DebugHandViewType view)
        {
            astra_debug_handstream_set_view_type(debugHandStream_, view);
        }

        void set_use_mouse_probe(bool useMouseProbe)
        {
            astra_debug_handstream_set_use_mouse_probe(debugHandStream_, useMouseProbe);
        }

        void set_mouse_position(vector2f position)
        {
            astra_debug_handstream_set_mouse_position(debugHandStream_, position);
        }

        void set_pause_input(bool pauseInput)
        {
            astra_debug_handstream_set_pause_input(debugHandStream_, pauseInput);
        }

        void set_lock_spawn_point(bool lockSpawnPoint)
        {
            astra_debug_handstream_set_lock_spawn_point(debugHandStream_, lockSpawnPoint);
        }

        DebugHandViewType get_view_type()
        {
            DebugHandViewType view;
            astra_debug_handstream_get_view_type(debugHandStream_, &view);
            return view;
        }

    private:
        astra_debug_handstream_t debugHandStream_;
    };

    class debug_handframe : public imageframe<rgb_pixel, ASTRA_STREAM_DEBUG_HAND>
    {
    public:
        debug_handframe(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_RGB888)
        {}
    };
}

#endif /* ASTRA_HAND_DEBUG_HPP */
