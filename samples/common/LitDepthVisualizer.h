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
            m_lightColor = {210,210,210};
            m_lightVector = {0.44022, -0.17609, 0.88045};;
            m_ambientColor = {30,30,30};
        }

        void set_light_color(sensekit_rgb_pixel_t color)
        {
            m_lightColor = color;
        }

        void set_light_direction(sensekit_vector3f_t direction)
        {
            m_lightVector = cvector_to_vector(direction);
        }

        void set_ambient_color(sensekit_rgb_pixel_t color)
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
        Vector3f* m_normalMap{nullptr};
        Vector3f* m_blurNormalMap{nullptr};

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
        sensekit::DepthStream  m_depthStream;

        void init_buffer(size_t width, size_t height);
        void calculate_normals(DepthFrame& frame);
    };

    void LitDepthVisualizer::calculate_normals(DepthFrame& frame)
    {
        const short* depthData;
        size_t depthLength;

        depthData = frame.data();
        depthLength = frame.byteLength();
        const CoordinateMapper& mapper = m_depthStream.coordinateMapper();

        int depthWidth = frame.resolutionX();
        int depthHeight = frame.resolutionY();

        int numPixels = depthWidth * depthHeight;
        if (m_normalMap == nullptr || m_normalMapLength != numPixels)
        {
            m_normalMap = new Vector3f[numPixels];
            m_blurNormalMap = new Vector3f[numPixels];
            memset(m_blurNormalMap, 0, sizeof(Vector3f)*numPixels);
            m_normalMapLength = numPixels;
        }

        Vector3f* normMap = m_normalMap;

        //top row
        for (int x = 0; x < depthWidth; ++x)
        {
            *normMap = Vector3f();
            ++normMap;
        }

        for (int y = 1; y < depthHeight - 1; ++y)
        {
            //first pixel at start of row
            *normMap = Vector3f();
            ++normMap;

            for (int x = 1; x < depthWidth - 1; ++x)
            {
                int index = x + y * depthWidth;
                int rightIndex = index + 1;
                int leftIndex = index - 1;
                int upIndex = index - depthWidth;
                int downIndex = index + depthWidth;

                int16_t depth = *(depthData + index);
                int16_t depthLeft = *(depthData + leftIndex);
                int16_t depthRight = *(depthData + rightIndex);
                int16_t depthUp = *(depthData + upIndex);
                int16_t depthDown = *(depthData + downIndex);

                Vector3f normAvg;

                if (depth != 0 && depthRight != 0 && depthDown != 0)
                {
                    float worldX1, worldY1, worldZ1;
                    float worldX2, worldY2, worldZ2;
                    float worldX3, worldY3, worldZ3;
                    mapper.convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                    mapper.convert_depth_to_world(x + 1, y, depthRight, &worldX2, &worldY2, &worldZ2);
                    mapper.convert_depth_to_world(x, y + 1, depthDown, &worldX3, &worldY3, &worldZ3);

                    Vector3f v1 = Vector3f(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                    Vector3f v2 = Vector3f(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                    Vector3f norm = v2.cross(v1);
                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }

                if (depth != 0 && depthRight != 0 && depthUp != 0)
                {
                    float worldX1, worldY1, worldZ1;
                    float worldX2, worldY2, worldZ2;
                    float worldX3, worldY3, worldZ3;
                    mapper.convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                    mapper.convert_depth_to_world(x, y - 1, depthUp, &worldX2, &worldY2, &worldZ2);
                    mapper.convert_depth_to_world(x + 1, y, depthRight, &worldX3, &worldY3, &worldZ3);

                    Vector3f v1 = Vector3f(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                    Vector3f v2 = Vector3f(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                    Vector3f norm = v2.cross(v1);

                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }

                if (depth != 0 && depthLeft != 0 && depthUp != 0)
                {
                    float worldX1, worldY1, worldZ1;
                    float worldX2, worldY2, worldZ2;
                    float worldX3, worldY3, worldZ3;
                    mapper.convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                    mapper.convert_depth_to_world(x - 1, y, depthLeft, &worldX2, &worldY2, &worldZ2);
                    mapper.convert_depth_to_world(x, y - 1, depthUp, &worldX3, &worldY3, &worldZ3);

                    Vector3f v1 = Vector3f(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                    Vector3f v2 = Vector3f(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                    Vector3f norm = v2.cross(v1);

                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }

                if (depth != 0 && depthLeft != 0 && depthDown != 0)
                {
                    float worldX1, worldY1, worldZ1;
                    float worldX2, worldY2, worldZ2;
                    float worldX3, worldY3, worldZ3;
                    mapper.convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                    mapper.convert_depth_to_world(x, y + 1, depthDown, &worldX2, &worldY2, &worldZ2);
                    mapper.convert_depth_to_world(x - 1, y, depthLeft, &worldX3, &worldY3, &worldZ3);

                    Vector3f v1 = Vector3f(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                    Vector3f v2 = Vector3f(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                    Vector3f norm = v2.cross(v1);

                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }

                *normMap = Vector3f::normalize(normAvg);
                ++normMap;
            }
            //last pixel at end of row
            *normMap = Vector3f();
            ++normMap;
        }
        //bottom row
        for (int x = 0; x < depthWidth; ++x)
        {
            *normMap = Vector3f();
            ++normMap;
        }

        const int blurRadius = m_blurRadius;
        //box blur
        for (int y = blurRadius; y < depthHeight - blurRadius; y++)
        {
            for (int x = blurRadius; x < depthWidth - blurRadius; x++)
            {
                Vector3f normAvg;

                for (int dy = -blurRadius; dy <= blurRadius; dy++)
                {
                    for (int dx = -blurRadius; dx <= blurRadius; dx++)
                    {
                        int index = x + dx + (y + dy) * depthWidth;
                        Vector3f norm = *(m_normalMap + index);

                        normAvg.x += norm.x;
                        normAvg.y += norm.y;
                        normAvg.z += norm.z;
                    }
                }
                int centerIndex = x + y*depthWidth;
                *(m_blurNormalMap + centerIndex) = Vector3f::normalize(normAvg);
            }
        }
    }

    void LitDepthVisualizer::init_buffer(size_t width, size_t height)
    {
        if (m_outputBuffer == nullptr || width != m_outputWidth || height != m_outputHeight)
        {
            m_outputWidth = width;
            m_outputHeight = height;
            m_outputBuffer = std::make_unique<sensekit_rgb_pixel_t[]>(m_outputWidth * m_outputHeight);
        }
    }

    void LitDepthVisualizer::update(sensekit::DepthFrame& frame)
    {
        calculate_normals(frame);

        int depthWidth = frame.resolutionX();
        int depthHeight = frame.resolutionY();

        init_buffer(depthWidth, depthHeight);
        std::fill(m_outputBuffer.get(), m_outputBuffer.get()+m_outputWidth*m_outputHeight, sensekit_rgb_pixel_t{0,0,0});

        const short* pDepthRow;
        size_t depthLength;

        pDepthRow = frame.data();
        depthLength = frame.byteLength();

        const CoordinateMapper& mapper = m_depthStream.coordinateMapper();

        sensekit_rgb_pixel_t* pTexRow = m_outputBuffer.get();
        int rowSize = depthWidth;

        Vector3f* normMap = m_blurNormalMap;
        bool showNormMap = normMap != nullptr;

        for (int y = 0; y < depthHeight; ++y)
        {
            const short* pDepth = pDepthRow;
            sensekit_rgb_pixel_t* pTex = pTexRow;

            for (int x = 0; x < depthWidth; ++x, ++pDepth, ++normMap, ++pTex)
            {
                int16_t depth = *pDepth;

                Vector3f norm(1,0,0);
                if (showNormMap)
                {
                    norm = *normMap;
                }
                if (!norm.is_zero())
                {
                    float fadeFactor = 1 - 0.6*std::max(0.0f, std::min(1.0f, ((depth - 400) / 3200.0f)));
                    float diffuseFactor = norm.dot(m_lightVector);
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

                    pTex->r = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.r + diffuseColor.r))));
                    pTex->g = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.g + diffuseColor.g))));
                    pTex->b = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.b + diffuseColor.b))));
                }
            }

            pDepthRow += rowSize;
            pTexRow += m_outputWidth;
        }
    }
}}

#endif /* LITDEPTHVISUALIZER_H */
