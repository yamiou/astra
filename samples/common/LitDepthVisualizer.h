#ifndef LITDEPTHVISUALIZER_H
#define LITDEPTHVISUALIZER_H

#include <Astra/astra_capi.h>
#include <AstraUL/streams/Point.h>
#include <AstraUL/Vector.h>
#include <cstring>
#include <algorithm>

namespace samples { namespace common {

    using namespace astra;

    class LitDepthVisualizer
    {
    public:
        LitDepthVisualizer()
            : m_lightVector(0.44022f, -0.17609f, 0.88045f)
        {
            m_lightColor = {210, 210, 210};
            m_ambientColor = {30, 30, 30};
        }

        void set_light_color(const astra_rgb_pixel_t& color)
        {
            m_lightColor = color;
        }

        void set_light_direction(const astra_vector3f_t& direction)
        {
            m_lightVector = direction;
        }

        void set_ambient_color(const astra_rgb_pixel_t& color)
        {
            m_ambientColor = color;
        }

        void set_blur_radius(unsigned int radius)
        {
            m_blurRadius = radius;
        }

        void update(PointFrame& pointFrame);

        astra_rgb_pixel_t* get_output() { return m_outputBuffer.get(); }

    private:
        using VectorMapPtr = std::unique_ptr<Vector3f[]>;
        VectorMapPtr m_normalMap{nullptr};
        VectorMapPtr m_blurNormalMap{nullptr};
        size_t m_normalMapLength{0};

        Vector3f m_lightVector;
        unsigned int m_blurRadius{1};
        astra_rgb_pixel_t m_lightColor;
        astra_rgb_pixel_t m_ambientColor;

        size_t m_outputWidth;
        size_t m_outputHeight;

        using BufferPtr = std::unique_ptr<astra_rgb_pixel_t[]>;
        BufferPtr m_outputBuffer{nullptr};

        void prepare_buffer(size_t width, size_t height);
        void calculate_normals(PointFrame& pointFrame);
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

    void LitDepthVisualizer::calculate_normals(PointFrame& pointFrame)
    {
        const Vector3f* positionMap = pointFrame.data();

        const int width = pointFrame.resolutionX();
        const int height = pointFrame.resolutionY();

        const int numPixels = width * height;

        if (m_normalMap == nullptr || m_normalMapLength != numPixels)
        {
            m_normalMap = std::make_unique<Vector3f[]>(numPixels);
            m_blurNormalMap = std::make_unique<Vector3f[]>(numPixels);

            std::fill(m_blurNormalMap.get(), m_blurNormalMap.get() + numPixels, Vector3f::zero());

            m_normalMapLength = numPixels;
        }

        Vector3f* normMap = m_normalMap.get();

        //top row
        for (int x = 0; x < width; ++x)
        {
            *normMap = Vector3f::zero();
            ++normMap;
        }

        for (int y = 1; y < height - 1; ++y)
        {
            //first pixel at start of row
            *normMap = Vector3f::zero();
            ++normMap;

            for (int x = 1; x < width - 1; ++x)
            {
                size_t index = x + y * width;
                size_t rightIndex = index + 1;
                size_t leftIndex = index - 1;
                size_t upIndex = index - width;
                size_t downIndex = index + width;

                Vector3f point = positionMap[index];
                Vector3f pointLeft = positionMap[leftIndex];
                Vector3f pointRight = positionMap[rightIndex];
                Vector3f pointUp = positionMap[upIndex];
                Vector3f pointDown = positionMap[downIndex];

                Vector3f normAvg;

                if (point.z != 0)
                {
                    if (pointRight.z != 0 && pointDown.z != 0)
                    {
                        Vector3f v1 = pointRight - point;
                        Vector3f v2 = pointDown - point;

                        Vector3f norm = v2.cross(v1);
                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (pointRight.z != 0 && pointUp.z != 0)
                    {
                        Vector3f v1 = pointUp - point;
                        Vector3f v2 = pointRight - point;

                        Vector3f norm = v2.cross(v1);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (pointLeft.z != 0 && pointUp.z != 0)
                    {
                        Vector3f v1 = pointLeft - point;
                        Vector3f v2 = pointUp - point;

                        Vector3f norm = v2.cross(v1);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }

                    if (pointLeft.z != 0 && pointDown.z != 0)
                    {
                        Vector3f v1 = pointDown - point;
                        Vector3f v2 = pointLeft - point;

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
            *normMap = Vector3f::zero();
            ++normMap;
        }

        //bottom row
        for (int x = 0; x < width; ++x)
        {
            *normMap = Vector3f::zero();
            ++normMap;
        }

        box_blur(m_normalMap.get(), m_blurNormalMap.get(), width, height, m_blurRadius);
    }

    void LitDepthVisualizer::prepare_buffer(size_t width, size_t height)
    {
        if (m_outputBuffer == nullptr || width != m_outputWidth || height != m_outputHeight)
        {
            m_outputWidth = width;
            m_outputHeight = height;
            m_outputBuffer = std::make_unique<astra_rgb_pixel_t[]>(m_outputWidth * m_outputHeight);
        }

        std::fill(m_outputBuffer.get(), m_outputBuffer.get()+m_outputWidth*m_outputHeight, astra_rgb_pixel_t{0,0,0});
    }

    void LitDepthVisualizer::update(astra::PointFrame& pointFrame)
    {
        calculate_normals(pointFrame);

        const size_t width = pointFrame.resolutionX();
        const size_t height = pointFrame.resolutionY();

        prepare_buffer(width, height);

        const Vector3f* pointData = pointFrame.data();

        astra_rgb_pixel_t* texturePtr = m_outputBuffer.get();

        const Vector3f* normMap = m_blurNormalMap.get();
        const bool useNormalMap = normMap != nullptr;

        for (unsigned y = 0; y < height; ++y)
        {
            for (unsigned x = 0; x < width; ++x, ++pointData, ++normMap, ++texturePtr)
            {
                float depth = (*pointData).z;

                Vector3f norm(1,0,0);

                if (useNormalMap)
                {
                    norm = *normMap;
                }

                if (!norm.is_zero())
                {
                    const float fadeFactor = static_cast<float>(
                        1.0f - 0.6f * std::max(0.0f, std::min(1.0f, ((depth - 400.0f) / 3200.0f))));

                    const float diffuseFactor = norm.dot(m_lightVector);

                    astra_rgb_pixel_t diffuseColor;

                    if (diffuseFactor > 0)
                    {
                        //only add diffuse when mesh is facing the light
                        diffuseColor.r = static_cast<uint8_t>(m_lightColor.r * diffuseFactor);
                        diffuseColor.g = static_cast<uint8_t>(m_lightColor.g * diffuseFactor);
                        diffuseColor.b = static_cast<uint8_t>(m_lightColor.b * diffuseFactor);
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
        }
    }
}}

#endif /* LITDEPTHVISUALIZER_H */
