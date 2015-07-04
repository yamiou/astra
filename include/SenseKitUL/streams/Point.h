#ifndef POINT_H
#define POINT_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/point_capi.h>
#include <SenseKitUL/streams/Image.h>

namespace sensekit {

    class PointStream : public DataStream
    {
    public:
        PointStream()
        {}

        explicit PointStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
        {
            m_pointStream = reinterpret_cast<sensekit_pointstream_t>(connection);
        }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_POINT;

    private:
        sensekit_pointstream_t m_pointStream;
    };

    class PointFrame : public ImageFrame<Vector3f, SENSEKIT_STREAM_POINT>
    {
    public:
        PointFrame(sensekit_imageframe_t frame)
            : ImageFrame(frame)
        {}
    };
}

#endif // POINT_H
