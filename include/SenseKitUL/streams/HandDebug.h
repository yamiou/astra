#ifndef HANDDEBUG_H
#define HANDDEBUG_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/streams/hand_capi.h>

namespace sensekit {
    
    class HandDebugStream : public DataStream
    {
    public:
        explicit HandDebugStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_HAND_DEBUG_IMAGE;
    };

    class HandDebugFrame : public ImageFrame<RGBPixel>
    {
    public:
        HandDebugFrame(sensekit_reader_frame_t readerFrame, sensekit_stream_subtype_t subtype)
            : ImageFrame(readerFrame, SENSEKIT_STREAM_HAND_DEBUG_IMAGE, subtype)
        { }
    };
}

#endif /* HANDDEBUG_H */
