#ifndef LITDEPTHVISUALIZER_H
#define LITDEPTHVISUALIZER_H

#include <SenseKit/sensekit_capi.h>
#include <SenseKitUL/streams/Depth.h>
#include <SenseKitUL/Vector.h>
#include <cstring>
#include <algorithm>

namespace samples { namespace common {

    using namespace sensekit;

    class LitDepthVisualizer
    {
    public:
        LitDepthVisualizer(DepthStream depthStream)
            : m_depthStream(depthStream)
        {
            m_lightColor = {210, 210, 210};
            m_lightVector = {0.44022f, -0.17609f, 0.88045f};
            m_ambientColor = {30, 30, 30};
        }

        void set_light_color(const sensekit_rgb_pixel_t& color)
        {
            m_lightColor = color;
        }

        void set_light_direction(const sensekit_vector3f_t& direction)
        {
            m_lightVector = cvector_to_vector(direction);
        }

        void set_ambient_color(const sensekit_rgb_pixel_t& color)
        {
            m_ambientColor = color;
        }

        void set_blur_radius(unsigned int radius)
        {
            m_blurRadius = radius;
        }

        void update(DepthFrame& frame);

        sensekit_rgb_pixel_t* get_output() { return m_outputBuffer.get(); }

    private:
        using VectorMapPtr = std::unique_ptr<Vector3f[]>;
        VectorMapPtr m_positionMap{nullptr};
        VectorMapPtr m_normalMap{nullptr};
        VectorMapPtr m_blurNormalMap{nullptr};
        size_t m_normalMapLength{0};

        Vector3f m_lightVector;
        unsigned int m_blurRadius{1};
        sensekit_rgb_pixel_t m_lightColor;
        sensekit_rgb_pixel_t m_ambientColor;

        size_t m_outputWidth;
        size_t m_outputHeight;
        size_t m_outputByteLength;

        using BufferPtr = std::unique_ptr<sensekit_rgb_pixel_t[]>;
        BufferPtr m_outputBuffer{nullptr};

        sensekit::DepthStream m_depthStream;

        void prepare_buffer(size_t width, size_t height);
        void calculate_normals(DepthFrame& frame);
    };

    void box_blur(const Vector3f* in,
                  Vector3f* out,
                  const size_t width,
                  const size_t height,
                  const int blurRadius = 1)
    {
        for (size_t y = blurRadius; y < height - blurRadius; y++)
        {
            for (size_t x = blurRadius; x < width - blurRadius; x++)
            {
                Vector3f normAvg;

                for (int dy = -blurRadius; dy <= blurRadius; dy++)
                {
                    for (int dx = -blurRadius; dx <= blurRadius; dx++)
                    {
                        size_t index = x + dx + (y + dy) * width;
                        Vector3f norm = in[index];

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }
                }

                const size_t centerIndex = x + y * width;
                out[centerIndex] = Vector3f::normalize(normAvg);
            }
        }
    }

    void LitDepthVisualizer::calculate_normals(DepthFrame& frame)
    {
        const short* depthData = frame.data();
        const CoordinateMapper& mapper = m_depthStream.coordinateMapper();
        const size_t depthLength = frame.byteLength();

        const int depthWidth = frame.resolutionX();
        const int depthHeight = frame.resolutionY();

        const int numPixels = depthWidth * depthHeight;

        if (m_normalMap == nullptr || m_normalMapLength != numPixels)
        {
            m_positionMap = std::make_unique<Vector3f[]>(numPixels);
            m_normalMap = std::make_unique<Vector3f[]>(numPixels);
            m_blurNormalMap = std::make_unique<Vector3f[]>(numPixels);

            std::fill(m_blurNormalMap.get(), m_blurNormalMap.get() + numPixels, Vector3f::zero);

            m_normalMapLength = numPixels;
        }

        Vector3f* positionMap = m_positionMap.get();

        for (int y = 0; y < depthHeight; y++)
        {
            for(int x = 0; x < depthWidth; x++)
            {
                const size_t index = x + y * depthWidth;
                const uint16_t depth = depthData[index];

                if (depth == 0)
                    continue;

                positionMap[index] = mapper.convert_depth_to_world(Vector3f(x,y, depthData[index]));
            }
        }

        Vector3f* normMap = m_normalMap.get();

        //top row
        for (int x = 0; x < depthWidth; ++x)
        {
            *normMap = Vector3f::zero;
            ++normMap;
        }

        for (int y = 1; y < depthHeight - 1; ++y)
        {
            //first pixel at start of row
            *normMap = Vector3f::zero;
            ++normMap;

            for (int x = 1; x < depthWidth - 1; ++x)
            {
                size_t index = x + y * depthWidth;
                size_t rightIndex = index + 1;
                size_t leftIndex = index - 1;
                size_t upIndex = index - depthWidth;
                size_t downIndex = index + depthWidth;

                int16_t depth = depthData[index];
                int16_t depthLeft = depthData[leftIndex];
                int16_t depthRight = depthData[rightIndex];
                int16_t depthUp = depthData[upIndex];
                int16_t depthDown = depthData[downIndex];

                Vector3f normAvg;

                if (depth != 0)
                {
                    if (depthRight != 0 && depthDown != 0)
                    {
                        Vector3f v1 = positionMap[rightIndex] - positionMap[index];
                        Vector3f v2 = positionMap[downIndex] - positionMap[index];

                        Vector3f norm = v2.cross(v1);
                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (depthRight != 0 && depthUp != 0)
                    {
                        Vector3f v1 = positionMap[upIndex] - positionMap[index];
                        Vector3f v2 = positionMap[rightIndex] - positionMap[index];

                        Vector3f norm = v2.cross(v1);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (depthLeft != 0 && depthUp != 0)
                    {
                        Vector3f v1 = positionMap[leftIndex] - positionMap[index];
                        Vector3f v2 = positionMap[upIndex] - positionMap[index];

                        Vector3f norm = v2.cross(v1);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (depthLeft != 0 && depthDown != 0)
                    {
                        Vector3f v1 = positionMap[downIndex] - positionMap[index];
                        Vector3f v2 = positionMap[leftIndex] - positionMap[index];

                        Vector3f norm = v2.cross(v1);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }
                }

                *normMap = Vector3f::normalize(normAvg);
                ++normMap;
            }

            //last pixel at end of row
            *normMap = Vector3f::zero;
            ++normMap;
        }

        //bottom row
        for (int x = 0; x < depthWidth; ++x)
        {
            *normMap = Vector3f::zero;
            ++normMap;
        }

        box_blur(m_normalMap.get(), m_blurNormalMap.get(), depthWidth, depthHeight, m_blurRadius);
    }

    void LitDepthVisualizer::prepare_buffer(size_t width, size_t height)
    {
        if (m_outputBuffer == nullptr || width != m_outputWidth || height != m_outputHeight)
        {
            m_outputWidth = width;
            m_outputHeight = height;
            m_outputBuffer = std::make_unique<sensekit_rgb_pixel_t[]>(m_outputWidth * m_outputHeight);
        }

        std::fill(m_outputBuffer.get(), m_outputBuffer.get()+m_outputWidth*m_outputHeight, sensekit_rgb_pixel_t{0,0,0});
    }

    void LitDepthVisualizer::update(sensekit::DepthFrame& frame)
    {
        calculate_normals(frame);

        const size_t depthWidth = frame.resolutionX();
        const size_t depthHeight = frame.resolutionY();

        prepare_buffer(depthWidth, depthHeight);

        const short* depthRowPtr = frame.data();
        const size_t depthLength = frame.byteLength();
        const CoordinateMapper& mapper = m_depthStream.coordinateMapper();

        sensekit_rgb_pixel_t* textureRowPtr = m_outputBuffer.get();

        const Vector3f* normMap = m_blurNormalMap.get();
        const bool useNormalMap = normMap != nullptr;

        for (int y = 0; y < depthHeight; ++y)
        {
            const short* depthPtr = depthRowPtr;
            sensekit_rgb_pixel_t* texturePtr = textureRowPtr;

            for (int x = 0; x < depthWidth; ++x, ++depthPtr, ++normMap, ++texturePtr)
            {
                int16_t depth = *depthPtr;

                Vector3f norm(1,0,0);

                if (useNormalMap)
                {
                    norm = *normMap;
                }

                if (!norm.is_zero())
                {
                    const float fadeFactor = 1 - 0.6*std::max(0.0f, std::min(1.0f, ((depth - 400) / 3200.0f)));
                    const float diffuseFactor = norm.dot(m_lightVector);

                    sensekit_rgb_pixel_t diffuseColor;

                    if (diffuseFactor > 0)
                    {
                        //only add diffuse when mesh is facing the light
                        diffuseColor.r = m_lightColor.r * diffuseFactor;
                        diffuseColor.g = m_lightColor.g * diffuseFactor;
                        diffuseColor.b = m_lightColor.b * diffuseFactor;
                    }
                    else
                    {
                        diffuseColor.r = 0;
                        diffuseColor.g = 0;
                        diffuseColor.b = 0;
                    }

                    texturePtr->r = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.r + diffuseColor.r))));
                    texturePtr->g = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.g + diffuseColor.g))));
                    texturePtr->b = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.b + diffuseColor.b))));
                }
            }

            depthRowPtr += depthWidth;
            textureRowPtr += m_outputWidth;
        }
    }
}}

#endif /* LITDEPTHVISUALIZER_H */
