#ifndef LITDEPTHVISUALIZER_H
#define LITDEPTHVISUALIZER_H

#include <astra/astra.hpp>
#include <cstring>
#include <algorithm>

namespace samples { namespace common {

    using namespace astra;

    class lit_depth_visualizer
    {
    public:
        lit_depth_visualizer()
            : lightVector_(0.44022f, -0.17609f, 0.88045f)
        {
            lightColor_ = {210, 210, 210};
            ambientColor_ = {30, 30, 30};
        }

        void set_light_color(const astra_rgb_pixel_t& color)
        {
            lightColor_ = color;
        }

        void set_light_direction(const astra_vector3f_t& direction)
        {
            lightVector_ = direction;
        }

        void set_ambient_color(const astra_rgb_pixel_t& color)
        {
            ambientColor_ = color;
        }

        void set_blur_radius(unsigned int radius)
        {
            blurRadius_ = radius;
        }

        void update(pointframe& pointFrame);

        astra_rgb_pixel_t* get_output() { return outputBuffer_.get(); }

    private:
        using VectorMapPtr = std::unique_ptr<vector3f[]>;
        VectorMapPtr normalMap_{nullptr};
        VectorMapPtr blurNormalMap_{nullptr};
        size_t normalMapLength_{0};

        vector3f lightVector_;
        unsigned int blurRadius_{1};
        astra_rgb_pixel_t lightColor_;
        astra_rgb_pixel_t ambientColor_;

        size_t outputWidth_;
        size_t outputHeight_;

        using BufferPtr = std::unique_ptr<astra_rgb_pixel_t[]>;
        BufferPtr outputBuffer_{nullptr};

        void prepare_buffer(size_t width, size_t height);
        void calculate_normals(pointframe& pointFrame);
    };

    void box_blur(const vector3f* in,
                  vector3f* out,
                  const size_t width,
                  const size_t height,
                  const int blurRadius = 1)
    {
        const size_t maxY = height - blurRadius;
        const size_t maxX = width - blurRadius;

        out += blurRadius * width;

        for (size_t y = blurRadius; y < maxY; ++y)
        {
            out += blurRadius;
            for (size_t x = blurRadius; x < maxX; ++x, ++out)
            {
                vector3f normAvg;

                size_t index = x - blurRadius + (y - blurRadius) * width;
                const vector3f* in_row = in + index;

                for (int dy = -blurRadius; dy <= blurRadius; ++dy, in_row += width)
                {
                    const vector3f* in_kernel = in_row;
                    for (int dx = -blurRadius; dx <= blurRadius; ++dx, ++in_kernel)
                    {
                        normAvg += *in_kernel;
                    }
                }

                *out = vector3f::normalize(normAvg);
            }
            out += blurRadius;
        }
    }

    void box_blur_fast(const vector3f* in,
                       vector3f* out,
                       const size_t width,
                       const size_t height)
    {
        const size_t maxY = height;
        const size_t maxX = width;

        const vector3f* in_row = in + width;
        vector3f* out_row = out;

        memset(out, 0, width * height * sizeof(vector3f));

        for (size_t y = 1; y < maxY; ++y, in_row += width, out_row += width)
        {
            const vector3f* in_left = in_row - 1;
            const vector3f* in_mid = in_row + 1;

            vector3f* out_up = out_row;
            vector3f* out_mid = out_row + width;

            vector3f xKernelTotal = *in_left + *in_row;

            for (size_t x = 1; x < maxX; ++x)
            {
                xKernelTotal += *in_mid;

                *out_up += xKernelTotal;
                *out_mid += xKernelTotal;

                xKernelTotal -= *in_left;

                ++in_left;
                ++in_mid;
                ++out_up;
                ++out_mid;
            }
        }
    }

    void lit_depth_visualizer::calculate_normals(pointframe& pointFrame)
    {
        const vector3f* positionMap = pointFrame.data();

        const int width = pointFrame.resolutionX();
        const int height = pointFrame.resolutionY();

        const int numPixels = width * height;

        if (normalMap_ == nullptr || normalMapLength_ != numPixels)
        {
            normalMap_ = std::make_unique<vector3f[]>(numPixels);
            blurNormalMap_ = std::make_unique<vector3f[]>(numPixels);

            std::fill(blurNormalMap_.get(), blurNormalMap_.get() + numPixels, vector3f::zero());

            normalMapLength_ = numPixels;
        }

        vector3f* normMap = normalMap_.get();

        //top row
        for (int x = 0; x < width; ++x)
        {
            *normMap = vector3f::zero();
            ++normMap;
        }

        const int maxY = height - 1;
        const int maxX = width - 1;

        for (int y = 1; y < maxY; ++y)
        {
            //first pixel at start of row
            *normMap = vector3f::zero();
            ++normMap;

            //Initialize pointer arithmetic for the x=0 position
            const vector3f* p_point = positionMap + y * width;
            const vector3f* p_pointLeft = p_point - 1;
            const vector3f* p_pointRight = p_point + 1;
            const vector3f* p_pointUp = p_point - width;
            const vector3f* p_pointDown = p_point + width;

            for (int x = 1; x < maxX; ++x)
            {
                ++p_pointLeft;
                ++p_point;
                ++p_pointRight;
                ++p_pointUp;
                ++p_pointDown;

                const vector3f& point = *p_point;
                const vector3f& pointLeft = *p_pointLeft;
                const vector3f& pointRight = *p_pointRight;
                const vector3f& pointUp = *p_pointUp;
                const vector3f& pointDown = *p_pointDown;

                if (point.z != 0 &&
                    pointRight.z != 0 &&
                    pointDown.z != 0 &&
                    pointLeft.z != 0 &&
                    pointUp.z != 0
                    )
                {
                    vector3f vr = pointRight - point;
                    vector3f vd = pointDown - point;
                    vector3f vl = pointLeft - point;
                    vector3f vu = pointUp - point;

                    vector3f normAvg = vd.cross(vr);
                    normAvg += vl.cross(vd);
                    normAvg += vu.cross(vl);
                    normAvg += vr.cross(vu);

                    *normMap = vector3f::normalize(normAvg);

                }
                else
                {
                    *normMap = vector3f::zero();
                }

                ++normMap;
            }

            //last pixel at end of row
            *normMap = vector3f::zero();
            ++normMap;
        }

        //bottom row
        for (int x = 0; x < width; ++x)
        {
            *normMap = vector3f::zero();
            ++normMap;
        }

        //box_blur(normalMap_.get(), blurNormalMap_.get(), width, height, blurRadius_);
        box_blur_fast(normalMap_.get(), blurNormalMap_.get(), width, height);
    }

    void lit_depth_visualizer::prepare_buffer(size_t width, size_t height)
    {
        if (outputBuffer_ == nullptr || width != outputWidth_ || height != outputHeight_)
        {
            outputWidth_ = width;
            outputHeight_ = height;
            outputBuffer_ = std::make_unique<astra_rgb_pixel_t[]>(outputWidth_ * outputHeight_);
        }

        std::fill(outputBuffer_.get(), outputBuffer_.get()+outputWidth_*outputHeight_, astra_rgb_pixel_t{0,0,0});
    }

    void lit_depth_visualizer::update(astra::pointframe& pointFrame)
    {
        calculate_normals(pointFrame);

        const size_t width = pointFrame.resolutionX();
        const size_t height = pointFrame.resolutionY();

        prepare_buffer(width, height);

        const vector3f* pointData = pointFrame.data();

        astra_rgb_pixel_t* texturePtr = outputBuffer_.get();

        const vector3f* normMap = blurNormalMap_.get();
        const bool useNormalMap = normMap != nullptr;

        for (unsigned y = 0; y < height; ++y)
        {
            for (unsigned x = 0; x < width; ++x, ++pointData, ++normMap, ++texturePtr)
            {
                float depth = (*pointData).z;

                vector3f norm(1,0,0);

                if (useNormalMap)
                {
                    norm = vector3f::normalize(*normMap);
                }

                if (depth != 0)
                {
                    const float fadeFactor = static_cast<float>(
                        1.0f - 0.6f * std::max(0.0f, std::min(1.0f, ((depth - 400.0f) / 3200.0f))));

                    const float diffuseFactor = norm.dot(lightVector_);

                    astra_rgb_pixel_t diffuseColor;

                    if (diffuseFactor > 0)
                    {
                        //only add diffuse when mesh is facing the light
                        diffuseColor.r = static_cast<uint8_t>(lightColor_.r * diffuseFactor);
                        diffuseColor.g = static_cast<uint8_t>(lightColor_.g * diffuseFactor);
                        diffuseColor.b = static_cast<uint8_t>(lightColor_.b * diffuseFactor);
                    }
                    else
                    {
                        diffuseColor.r = 0;
                        diffuseColor.g = 0;
                        diffuseColor.b = 0;
                    }

                    texturePtr->r = std::max(0, std::min(255, (int)(fadeFactor*(ambientColor_.r + diffuseColor.r))));
                    texturePtr->g = std::max(0, std::min(255, (int)(fadeFactor*(ambientColor_.g + diffuseColor.g))));
                    texturePtr->b = std::max(0, std::min(255, (int)(fadeFactor*(ambientColor_.b + diffuseColor.b))));
                }
                else
                {
                    texturePtr->r = 0;
                    texturePtr->g = 0;
                    texturePtr->b = 0;
                }
            }
        }
    }
}}

#endif /* LITDEPTHVISUALIZER_H */
