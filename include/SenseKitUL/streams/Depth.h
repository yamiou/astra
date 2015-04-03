#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include <StreamTypes.h>
#include "depth_capi.h"

namespace sensekit {

    class DepthStream : public DataStream
    {
    public:
        explicit DepthStream(sensekit_streamconnection_t* connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_DEPTH;
    };

    class DepthFrame
    {
    public:
        DepthFrame(sensekit_reader_frame_t* readerFrame)
            {
                sensekit_depth_frame_get(readerFrame, &m_depthFrame);
            }

        int get_resolutionX() { return m_depthFrame->width; }
        int get_resolutionY() { return m_depthFrame->height; }
        int get_frameIndex() { return m_depthFrame->frameIndex; }

        int16_t* data() { return m_depthFrame->data; }

    private:
        sensekit_depthframe_t* m_depthFrame;
    };
}

#endif /* DEPTH_H */
