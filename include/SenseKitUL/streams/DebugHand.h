#ifndef HANDDEBUG_H
#define HANDDEBUG_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
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

        DebugHandViewType get_view_type()
        {
            DebugHandViewType view;
            sensekit_debug_handstream_get_view_type(m_debugHandStream, &view);
            return view;
        }

    private:
        sensekit_debug_handstream_t m_debugHandStream;
    };

    class DebugHandFrame : public ImageFrame<RGBPixel>
    {
    public:
        DebugHandFrame(sensekit_reader_frame_t readerFrame, sensekit_stream_subtype_t subtype)
            : ImageFrame(readerFrame, SENSEKIT_STREAM_DEBUG_HAND, subtype)
        { }
    };
}

#endif /* HANDDEBUG_H */
