#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include <StreamTypes.h>
#include "depth_capi.h"

namespace sensekit {

    class DepthStream
    {
    public:
        explicit DepthStream(sensekit_streamconnection_t* connection)
            {
                m_connection = connection;
                m_pStreamConnection = reinterpret_cast<sensekit_depthstream_t*>(connection);
            }

        static const sensekit_stream_type_t id = sensekit_core_stream_types::SENSEKIT_STREAM_DEPTH;

        void start()
            {
                sensekit_stream_start(m_connection);
            }
        void stop()
            {
                sensekit_stream_stop(m_connection);
            }

    private:
        sensekit_streamconnection_t* m_connection;
        sensekit_depthstream_t* m_pStreamConnection;
    };

    class DepthFrame
    {
    public:
        DepthFrame(sensekit_reader_frame_t* readerFrame)
            {
                sensekit_depth_frame_get(readerFrame, &m_depthFrame);
            }

        int getResolutionX() { return m_depthFrame->width; }
        int getResolutionY() { return m_depthFrame->height; }
        int getFrameIndex() { return m_depthFrame->frameIndex; }

        int16_t* data() { return m_depthFrame->data; }

    private:
        sensekit_depthframe_t* m_depthFrame;
    };
}

#endif /* DEPTH_H */
