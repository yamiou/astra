#ifndef HANDDEBUG_H
#define HANDDEBUG_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/hand_capi.h>

namespace sensekit {

    using DebugHandViewType = sensekit_debug_hand_view_type_t;

    class DebugHandStream : public DataStream
    {
    public:
        explicit DebugHandStream(sensekit_streamconnection_t connection)
            : DataStream(connection),
            m_debugHandStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_DEBUG_HAND;

        void set_view_type(DebugHandViewType view)
        {
            sensekit_debug_handstream_set_view_type(m_debugHandStream, view);
        }

        void set_use_mouse_probe(bool useMouseProbe)
        {
            sensekit_debug_handstream_set_use_mouse_probe(m_debugHandStream, useMouseProbe);
        }

        void set_mouse_position(Vector2f position)
        {
            sensekit_debug_handstream_set_mouse_position(m_debugHandStream, position);
        }

        void set_pause_input(bool pauseInput)
        {
            sensekit_debug_handstream_set_pause_input(m_debugHandStream, pauseInput);
        }

        void spawn_point()
        {
            sensekit_debug_handstream_spawn_point(m_debugHandStream);
        }

        DebugHandViewType get_view_type()
        {
            DebugHandViewType view;
            sensekit_debug_handstream_get_view_type(m_debugHandStream, &view);
            return view;
        }

    private:
        sensekit_debug_handstream_t m_debugHandStream;
    };

    class DebugHandFrame : public ImageFrame<RGBPixel, SENSEKIT_STREAM_DEBUG_HAND>
    {
    public:
        DebugHandFrame(sensekit_imageframe_t frame)
            : ImageFrame(frame)
        {}
    };
}

#endif /* HANDDEBUG_H */
