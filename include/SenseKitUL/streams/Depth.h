#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include <stdexcept>
#include <StreamTypes.h>
#include "depth_capi.h"
#include <cmath>

namespace sensekit {

    class CoordinateMapper
    {
    public:
        CoordinateMapper(float horizontalFov, float verticalFov, int resolutionX, int resolutionY)
            {
                refresh_conversion_cache(horizontalFov, verticalFov, resolutionX, resolutionY);
            }

        void convert_depth_to_world(float depthX, float depthY, float depthZ,
                                    float* worldX, float* worldY, float* worldZ)
            {
                float normalizedX = depthX / m_conversionCache.resolutionX - .5f;
                float normalizedY = .5f - depthY / m_conversionCache.resolutionY;

                *worldX = normalizedX * depthZ * m_conversionCache.xzFactor;
                *worldY = normalizedY * depthZ * m_conversionCache.yzFactor;
                *worldZ = depthZ;
            }

        void convert_world_to_depth(float worldX, float worldY, float worldZ,
                                    float* depthX, float* depthY, float* depthZ)
            {
                *depthX = m_conversionCache.coeffX * worldX / worldZ + m_conversionCache.halfResX;
                *depthY = m_conversionCache.halfResY - m_conversionCache.coeffY * worldY / worldZ;
                *depthZ = worldZ;
            }

    private:
        void refresh_conversion_cache(float horizontalFov,
                                      float verticalFov,
                                      int resolutionX,
                                      int resolutionY)
            {
                m_conversionCache.xzFactor = tan(horizontalFov / 2) * 2;
                m_conversionCache.yzFactor = tan(verticalFov / 2) * 2;
                m_conversionCache.resolutionX = resolutionX;
                m_conversionCache.resolutionY = resolutionY;
                m_conversionCache.halfResX = m_conversionCache.resolutionX / 2;
                m_conversionCache.halfResY = m_conversionCache.resolutionY / 2;
                m_conversionCache.coeffX = m_conversionCache.resolutionX / m_conversionCache.xzFactor;
                m_conversionCache.coeffY = m_conversionCache.resolutionY / m_conversionCache.yzFactor;
            }

        conversion_cache_t m_conversionCache;
    };

    class DepthStream : public DataStream
    {
    public:
        explicit DepthStream(sensekit_streamconnection_t connection)
            : DataStream(connection),
              m_coordinateMapper(58.0f, 45.0f, 320, 200) // still the hardest of codings
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_DEPTH;

        CoordinateMapper& get_coordinateMapper();
    private:
        CoordinateMapper m_coordinateMapper;
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
