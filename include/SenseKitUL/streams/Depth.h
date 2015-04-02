#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include "depth_capi.h"

namespace sensekit {

    class DepthStream
    {
    public:
        explicit DepthStream(sensekit_streamconnection_t* connection)
            {
                m_pStreamConnection = reinterpret_cast<sensekit_depthstream_t*>(connection);
            }

        static const sensekit_stream_type_t id = 0;

        void start()
            {
                sensekit_depth_start(m_pStreamConnection);
            }
        void stop()
            {
                sensekit_depth_stop(m_pStreamConnection);
            }

    private:
        sensekit_depthstream_t* m_pStreamConnection;
    };

    class DepthFrame
    {
    public:
        DepthFrame()
            {

            }

        int getResolutionX() { return 0; }
        int getResolutionY() { return 0; }
        int getFrameIndex() { return 0; }

        uint16_t* data() { return nullptr; }
    };
}

#endif /* DEPTH_H */
