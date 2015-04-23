#ifndef DEBUGVISUALIZER_H
#define DEBUGVISUALIZER_H

#include <vector>
#include "TrackedPoint.h"
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit { namespace plugins { namespace hand {

    using namespace std;

    class DebugVisualizer
    {
    public:
        DebugVisualizer() {}
        ~DebugVisualizer() {}

        void overlayCrosshairs(const vector<TrackedPoint>& points,
                               _sensekit_imageframe& imageFrame)
        {
            int width = imageFrame.metadata.width;
            int height = imageFrame.metadata.height;

            uint8_t* colorData = static_cast<uint8_t*>(imageFrame.data);
            uint8_t bytesPerPixel = imageFrame.metadata.bytesPerPixel;

            for (auto iter = points.begin(); iter != points.end(); ++iter)
            {
                TrackedPoint tracked = *iter;
                cv::Point position = tracked.m_position;

                bool isActivePoint = tracked.m_type == TrackedPointType::ActivePoint;
                bool isLostTrackingPoint = isActivePoint && tracked.m_status == TrackingStatus::Lost;

                int y0 = MAX(0, position.y - 1);
                int y1 = MIN(height - 1, position.y + 1);
                int x0 = MAX(0, position.x - 1);
                int x1 = MIN(width - 1, position.x + 1);

                for (int y = y0; y <= y1; y++)
                {
                    for (int x = x0; x <= x1; x++)
                    {
                        uint8_t r = 0;
                        uint8_t g = 0;
                        uint8_t b = 0;

                        if ((y == position.y || x == position.x))
                        {
                            if (isLostTrackingPoint)
                            {
                                r = 255;
                                g = 0;
                                b = 0;
                            }
                            else if (isActivePoint)
                            {
                                r = 0;
                                g = 0;
                                b = 255;
                            }
                            else
                            {
                                r = 228;
                                g = 228;
                                b = 255;
                            }
                        }
                        else
                        {
                            r = 0;
                            g = 0;
                            b = 0;
                        }

                        int index = x + y * width;
                        uint8_t* pixel = colorData + index * bytesPerPixel;

                        *(pixel) = r;
                        *(pixel + 1) = g;
                        *(pixel + 2) = b;
                    }
                }
            }
        }

        void showDepthMat(const cv::Mat& matDepth,
                          const cv::Mat& matForeground,
                          _sensekit_imageframe& imageFrame)
        {
            assert(matDepth.cols == imageFrame.metadata.width);
            assert(matDepth.rows == imageFrame.metadata.height);
            assert(matDepth.size() == matForeground.size());

            int width = matDepth.cols;
            int height = matDepth.rows;

            uint8_t* colorData = static_cast<uint8_t*>(imageFrame.data);
            uint8_t bytesPerPixel = imageFrame.metadata.bytesPerPixel;

            for (int y = 0; y < height; ++y)
            {
                const float* depthRow = matDepth.ptr<float>(y);
                const char* foregroundRow = matForeground.ptr<char>(y);

                for (int x = 0; x < width; ++x, ++depthRow, ++foregroundRow, colorData += bytesPerPixel)
                {
                    uint8_t r = 0;
                    uint8_t g = 0;
                    uint8_t b = 0;

                    char foreground = 0;
                    if (m_showForeground)
                    {
                        foreground = *foregroundRow;
                    }
                    float depth = *depthRow;
                    uint8_t value = 255 * (depth / 4000.0f);

                    g = value;
                    b = value;

                    if (foreground == PixelType::Foreground)
                    {
                        r = 255;
                    }
                    else if (foreground == PixelType::Searched)
                    {
                        r = 128;
                        b = 255;
                    }
                    else
                    {
                        r = 0;
                    }

                    *(colorData) = r;
                    *(colorData + 1) = g;
                    *(colorData + 2) = b;
                }
            }
        }

        void showVelocityMat(const cv::Mat& matVelocity,
                             float maxScale,
                             const cv::Mat& matForeground,
                             _sensekit_imageframe& imageFrame)
        {
            assert(matVelocity.cols == imageFrame.metadata.width);
            assert(matVelocity.rows == imageFrame.metadata.height);
            assert(matVelocity.size() == matForeground.size());

            if (maxScale == 0)
            {
                throw new std::invalid_argument("maxScale cannot be 0");
            }

            int width = matVelocity.cols;
            int height = matVelocity.rows;

            uint8_t* colorData = static_cast<uint8_t*>(imageFrame.data);
            uint8_t bytesPerPixel = imageFrame.metadata.bytesPerPixel;

            for (int y = 0; y < height; ++y)
            {
                const float* velocityRow = matVelocity.ptr<float>(y);
                const char* foregroundRow = matForeground.ptr<char>(y);
                for (int x = 0; x < width; ++x, ++velocityRow, ++foregroundRow, colorData += bytesPerPixel)
                {
                    float velocity = *velocityRow;
                    char foreground = *foregroundRow;

                    int bvalue = 0;
                    if (foreground != PixelType::Background)
                    {
                        bvalue = 255;
                    }

                    int gvalue = static_cast<int>(255 * sqrt(min(1.0f, abs(velocity / maxScale))));
                    int rvalue = 0;
                    if (velocity < 0)
                    {
                        rvalue = gvalue;
                        gvalue = 0;
                    }


                    *(colorData) = bvalue;
                    *(colorData + 1) = gvalue;
                    *(colorData + 2) = rvalue;
                }
            }
        }

        template<typename T>
        void showNormArray(const cv::Mat& mat,
                           const cv::Mat& mask,
                           _sensekit_imageframe& imageFrame)
        {
            assert(mat.cols == imageFrame.metadata.width);
            assert(mat.rows == imageFrame.metadata.height);
            assert(mat.size() == mask.size());

            int width = mat.cols;
            int height = mat.rows;

            double min, max;
            cv::Point minLoc, maxLoc;
            cv::minMaxLoc(mat, &min, &max, &minLoc, &maxLoc , mask);

            double range = max - min;
            bool rangeZero = abs(range) < 0.00001;

            uint8_t* colorData = static_cast<uint8_t*>(imageFrame.data);
            const uint8_t bytesPerPixel = imageFrame.metadata.bytesPerPixel;

            for (int y = 0; y < height; ++y)
            {
                const T* dataRow = mat.ptr<T>(y);
                const uint8_t* maskRow = mask.ptr<uint8_t>(y);

                for (int x = 0; x < width; ++x, ++dataRow, ++maskRow, colorData += bytesPerPixel)
                {
                    float data = *dataRow;
                    uint8_t maskValue = *maskRow;
                    float value = 1;
                    if (0 == data || 0 == maskValue)
                    {
                        value = 0;
                    }
                    else if (rangeZero)
                    {
                        value = 255;
                    }
                    else
                    {
                        value = 55 + 200 * ((*dataRow - min) / range);
                    }
                    *(colorData) = value;
                    *(colorData + 1) = value;
                    *(colorData + 2) = value;
                }
            }
        }
    private:
        bool m_showForeground { true };

    };
}}}

#endif // DEBUGVISUALIZER_H
