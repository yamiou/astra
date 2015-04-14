#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/StreamTypes.h>
#include "depth_capi.h"
#include <cmath>

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

        float get_horizontalFieldOfView()
            {
                float hFov;
                sensekit_depth_stream_get_hfov(m_depthStream, &hFov);

                return hFov;
            }

        float get_verticalFieldOfView()
            {
                float vFov;
                sensekit_depth_stream_get_vfov(m_depthStream, &vFov);

                return vFov;
            }
    private:
        CoordinateMapper m_coordinateMapper;
        sensekit_depthstream_t m_depthStream;
    };

    class DepthFrame
    {
    public:
        DepthFrame(sensekit_reader_frame_t readerFrame)
            {
                if (readerFrame != nullptr)
                {
                    sensekit_depth_frame_get(readerFrame, &m_depthFrame);
                    sensekit_depthframe_get_metadata(m_depthFrame, &m_metadata);
                    sensekit_depthframe_get_frameindex(m_depthFrame, &m_frameIndex);
                    sensekit_depthframe_get_data_ptr(m_depthFrame, &m_dataPtr, &m_dataLength);
                }
            }

        bool is_valid() { return m_depthFrame != nullptr; }
        int get_resolutionX() { throwIfInvalidFrame(); return m_metadata.width; }
        int get_resolutionY() { throwIfInvalidFrame(); return m_metadata.height; }
        sensekit_frame_index_t get_frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }
        int get_bytesPerPixel() { throwIfInvalidFrame(); return m_metadata.bytesPerPixel; }

        const int16_t* data() { throwIfInvalidFrame(); return m_dataPtr; }
        size_t length() { throwIfInvalidFrame(); return m_dataLength; }

        void copy_to(int16_t* buffer)
            {
                throwIfInvalidFrame();
                sensekit_depthframe_copy_data(m_depthFrame, buffer);
            }

    private:
        void throwIfInvalidFrame()
            {
                if (m_depthFrame == nullptr)
                {
                    throw std::logic_error("Cannot operate on an invalid frame");
                }
            }
        sensekit_depthframe_t m_depthFrame{nullptr};
        sensekit_depthframe_metadata_t m_metadata;
        sensekit_frame_index_t m_frameIndex;
        int16_t* m_dataPtr;
        size_t m_dataLength;
    };


}

#endif /* DEPTH_H */
