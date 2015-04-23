#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/depth_capi.h>
#include <SenseKitUL/streams/Image.h>
#include <SenseKitUL/Vector.h>

namespace sensekit {

    class CoordinateMapper
    {
    public:
        CoordinateMapper(sensekit_depthstream_t depthStream)
            : m_depthStream(depthStream)
        { }

        Vector3f convert_depth_to_world(sensekit::Vector3f depthPosition) const
        {
            float worldX, worldY, worldZ;
            sensekit_convert_depth_to_world(m_depthStream,
                                            depthPosition.x,
                                            depthPosition.y,
                                            depthPosition.z,
                                            &worldX, &worldY, &worldZ);

            return Vector3f(worldX, worldY, worldZ);
        }

        Vector3f convert_world_to_depth(Vector3f worldPosition) const
        {
            float depthX, depthY, depthZ;
            sensekit_convert_world_to_depth(m_depthStream,
                                            worldPosition.x,
                                            worldPosition.y,
                                            worldPosition.z,
                                            &depthX, &depthY, &depthZ);

            return Vector3f(depthX, depthY, depthZ);
        }

        void convert_depth_to_world(float  depthX, float  depthY, float  depthZ,
                                    float* worldX, float* worldY, float* worldZ) const
        {
            sensekit_convert_depth_to_world(m_depthStream, depthX, depthY, depthZ,
                                            worldX, worldY, worldZ);
        }

        void convert_world_to_depth(float  worldX, float  worldY, float  worldZ,
                                    float* depthX, float* depthY, float* depthZ) const
        {
            sensekit_convert_world_to_depth(m_depthStream,
                                            worldX, worldY, worldZ,
                                            depthX, depthY, depthZ);
        }

    private:
        sensekit_depthstream_t m_depthStream;
    };

    class DepthStream : public DataStream
    {
    public:
        explicit DepthStream(sensekit_streamconnection_t connection)
            : DataStream(connection),
              m_depthStream(reinterpret_cast<sensekit_depthstream_t>(connection)),
              m_coordinateMapper(reinterpret_cast<sensekit_depthstream_t>(connection))
        { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_DEPTH;

        const CoordinateMapper& coordinateMapper() const { return m_coordinateMapper; };

        float horizontalFieldOfView()
        {
            float hFov;
            sensekit_depthstream_get_hfov(m_depthStream, &hFov);

            return hFov;
        }

        float verticalFieldOfView()
        {
            float vFov;
            sensekit_depthstream_get_vfov(m_depthStream, &vFov);

            return vFov;
        }

    private:
        CoordinateMapper m_coordinateMapper;
        sensekit_depthstream_t m_depthStream;
    };

    class DepthFrame : public ImageFrame<int16_t>
    {
    public:
        DepthFrame(sensekit_reader_frame_t readerFrame,
                   sensekit_stream_subtype_t subtype)
            : ImageFrame(readerFrame, SENSEKIT_STREAM_DEPTH, subtype),
              m_coordinateMapper(streamHandle())
        { }

        const CoordinateMapper& coordinateMapper() const { return m_coordinateMapper; };

    private:
         CoordinateMapper m_coordinateMapper;
    };
}

#endif /* DEPTH_H */
