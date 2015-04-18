#ifndef HANDDEBUG_H
#define HANDDEBUG_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/streams/hand_capi.h>

namespace sensekit {
    
    class DebugHandStream : public DataStream
    {
    public:
        explicit DebugHandStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_DEBUG_HAND;
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
