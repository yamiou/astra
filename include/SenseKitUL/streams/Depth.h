#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/StreamTypes.h>
#include "depth_capi.h"
#include <SenseKitUL/streams/Image.h>

namespace sensekit {

    class CoordinateMapper
    {
    public:
        CoordinateMapper(sensekit_depthstream_t depthStream)
            : m_depthStream(depthStream) {}
        void convert_depth_to_world(float depthX, float depthY, float depthZ,
                                    float* worldX, float* worldY, float* worldZ) const
        {
            sensekit_convert_depth_to_world(m_depthStream, depthX, depthY, depthZ,
                                            worldX, worldY, worldZ);
        }

        void convert_world_to_depth(float worldX, float worldY, float worldZ,
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

        const CoordinateMapper& get_coordinateMapper() const { return m_coordinateMapper; };

        float horizontalFieldOfView()
        {
            float hFov;
            sensekit_depth_stream_get_hfov(m_depthStream, &hFov);

            return hFov;
        }

        float verticalFieldOfView()
        {
            float vFov;
            sensekit_depth_stream_get_vfov(m_depthStream, &vFov);

            return vFov;
        }
    private:
        CoordinateMapper m_coordinateMapper;
        sensekit_depthstream_t m_depthStream;
    };

    class DepthFrame : public ImageFrame<int16_t>
    {
    public:
        DepthFrame(sensekit_reader_frame_t readerFrame, sensekit_stream_subtype_t subtype)
            : ImageFrame(readerFrame, SENSEKIT_STREAM_DEPTH, subtype)
        { }
    };
}

#endif /* DEPTH_H */
