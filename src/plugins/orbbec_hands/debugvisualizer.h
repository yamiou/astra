#ifndef DEBUGVISUALIZER_H
#define DEBUGVISUALIZER_H
#include <vector>
#include "trackedpoint.h"
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit
{
    namespace plugins
    {
        namespace hands
        {
            using namespace std;

            class DebugVisualizer
            {
            public:
                DebugVisualizer() {}
                ~DebugVisualizer() {}

                void overlayCrosshairs(vector<TrackedPoint>& points,
                                      _sensekit_colorframe& colorFrame)
                {
                    int width = colorFrame.metadata.width;
                    int height = colorFrame.metadata.height;

                    uint8_t* colorData = colorFrame.data;
                    uint8_t bytesPerPixel = colorFrame.metadata.bytesPerPixel;

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

                                if ((g == position.y || r == position.x))
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

                void showDepthMat(cv::Mat& matDepth,
                                  cv::Mat& matForeground,
                                  vector<TrackedPoint>& points,
                                  _sensekit_colorframe& colorFrame)
                {
                    assert(matDepth.cols == colorFrame.metadata.width);
                    assert(matDepth.rows == colorFrame.metadata.height);
                    assert(matDepth.size() == matForeground.size());

                    int width = matDepth.cols;
                    int height = matDepth.rows;

                    uint8_t* colorData = colorFrame.data;
                    uint8_t bytesPerPixel = colorFrame.metadata.bytesPerPixel;

                    bool m_showForeground = false;

                    for (int y = 0; y < height; ++y)
                    {
                        float* depthRow = matDepth.ptr<float>(y);
                        char* foregroundRow = matForeground.ptr<char>(y);

                        for (int x = 0; x < width; ++x)
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
                            int value = 255 * ((depth - 400.0f) / 3200.0f);

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
                            else if (foreground == PixelType::IntermediateClosest)
                            {
                                r = 0;
                                g = 0;
                                b = 255;
                            }
                            else if (foreground == PixelType::Closest)
                            {
                                r = 0;
                                g = 0;
                                b = 0;
                            }
                            else if (foreground == PixelType::Neighborhood)
                            {
                                r = 0;
                                g = 0;
                            }
                            else
                            {
                                r = 0;
                            }

                            *(colorData) = r;
                            *(colorData + 1) = g;
                            *(colorData + 2) = b;

                            ++depthRow;
                            ++foregroundRow;
                            colorData += bytesPerPixel;
                        }
                    }

                    overlayCrosshairs(points, colorFrame);
                }

                void showVelocityMat(cv::Mat& matVelocity,
                                     float maxScale,
                                     cv::Mat& matForeground,
                                     vector<TrackedPoint>& points,
                                     _sensekit_colorframe& colorFrame)
                {
                    assert(matVelocity.cols == colorFrame.metadata.width);
                    assert(matVelocity.rows == colorFrame.metadata.height);
                    assert(matVelocity.size() == matForeground.size());

                    if (maxScale == 0)
                    {
                        throw new std::invalid_argument("maxScale cannot be 0");
                    }

                    int width = matVelocity.cols;
                    int height = matVelocity.rows;

                    uint8_t* colorData = colorFrame.data;
                    uint8_t bytesPerPixel = colorFrame.metadata.bytesPerPixel;

                    for (int y = 0; y < height; ++y)
                    {
                        float* velocityRow = matVelocity.ptr<float>(y);
                        char* foregroundRow = matForeground.ptr<char>(y);
                        for (int x = 0; x < width; ++x)
                        {
                            float velocity = *velocityRow;
                            char foreground = *foregroundRow;

                            int bvalue = 0;
                            if (foreground == PixelType::Foreground)
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

                            ++velocityRow;
                            ++foregroundRow;
                            colorData += bytesPerPixel;
                        }
                    }

                    overlayCrosshairs(points, colorFrame);
                }
                /*
                  template<typename T>
                  void DebugVisualizer::showNormArray<T>(cv::Mat& mat,
                  cv::Mat& mask,
                  vector<TrackedPoint>& points,
                  _sensekit_colorframe& colorFrame)
                  {
                  assert(mat.cols == colorFrame.metadata.width);
                  assert(mat.rows == colorFrame.metadata.height);
                  assert(mat.size() == mask.size());

                  int width = mat.cols;
                  int height = mat.rows;

                  double min, max;
                  cv::Point minLoc, maxLoc;
                  cv::minMaxLoc(mat, &min, &max, &minLoc, &maxLoc, mask);

                  double range = max - min;
                  double rangeFactor = 0;
                  if (range > 0)
                  {
                  rangeFactor = 255.0 / range;
                  }
                  uint8_t* colorData = colorFrame.data;
                  uint8_t bytesPerPixel = colorFrame.metadata.bytesPerPixel;

                  for (int y = 0; y < height; ++y)
                  {
                  T* row = mat.ptr<T>(y);

                  for (int x = 0; x < width; ++x)
                  {
                  float data = *row;

                  float value = 1;
                  if (0 == data)
                  {
                  value = 0;
                  }
                  else if (0 == range)
                  {
                  value = 255;
                  }
                  else
                  {
                  value = 255 * ((*row - min) / range);
                  }
                  *(colorData    ) = value;
                  *(colorData + 1) = value;
                  *(colorData + 2) = value;

                  ++row;
                  colorData += bytesPerPixel;
                  }
                  }

                  overlayCrosshairs(points, colorFrame);
                  }
                  */

            private:

            };
        }
    }
}

#endif // DEBUGVISUALIZER_H
