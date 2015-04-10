#include <opencv2/opencv.hpp>
#include <SenseKitUL.h>
#include "depthutility.h"
#include "trackingdata.h"
namespace sensekit
{
    namespace plugins
    {
        namespace hands
        {
            DepthUtility::DepthUtility(int width, int height) :
                m_processingWidth(width),
                m_processingHeight(height),
                m_depthSmoothingFactor(0.05),
                m_foregroundThresholdFactor(0.02),
                m_maxDepthJumpPercent(0.1)
            {
                int erodeNum = 1;
                m_rectElement = cv::getStructuringElement(cv::MORPH_RECT, cv::Size(erodeNum * 2 + 1, erodeNum * 2 + 1), cv::Point(erodeNum, erodeNum));

                reset();
            }

            DepthUtility::~DepthUtility()
            {
            }

            void DepthUtility::reset()
            {
                m_matDepthPrevious = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
                m_matDepthAvg = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
                m_matDepthVel.create(m_processingHeight, m_processingWidth, CV_32FC1);
                m_matDepthVelErode.create(m_processingHeight, m_processingWidth, CV_32FC1);
            }

            void DepthUtility::processDepthToForeground(DepthFrame& depthFrame, cv::Mat& matDepth, cv::Mat& matForeground)
            {
                int width = depthFrame.get_resolutionX();
                int height = depthFrame.get_resolutionY();

                matDepth.create(m_processingHeight, m_processingWidth, CV_32FC1);
                matForeground = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_8UC1);

                depthFrameToMat(depthFrame, width, height, m_matDepthOriginal);

                //convert to the target processing size with nearest neighbor

                cv::resize(m_matDepthOriginal, matDepth, matDepth.size(), 0, 0, CV_INTER_NN);

                //current minus average, scaled by average = velocity as a percent change

                m_matDepthVel = (matDepth - m_matDepthAvg) / m_matDepthAvg;

                //accumulate current frame to average using smoothing factor

                cv::accumulateWeighted(matDepth, m_matDepthAvg, m_depthSmoothingFactor);

                filterZeroValuesAndJumps(matDepth, m_matDepthPrevious, m_matDepthAvg, m_matDepthVel, m_maxDepthJumpPercent);

                //erode to eliminate single pixel velocity artifacts

                cv::erode(abs(m_matDepthVel), m_matDepthVelErode, m_rectElement);

                thresholdForeground(matForeground, m_matDepthVelErode, m_foregroundThresholdFactor);
            }

            void DepthUtility::depthFrameToMat(DepthFrame& depthFrameSrc, int width, int height, cv::Mat& matTarget)
            {
                //ensure initialized
                matTarget.create(height, width, CV_32FC1);

                const int16_t* depthData = depthFrameSrc.data();
                size_t length = depthFrameSrc.length();

                for (int y = 0; y < height; ++y)
                {
                    float* row = matTarget.ptr<float>(y);
                    for (int x = 0; x < width; ++x)
                    {
                        row[x] = static_cast<float>(*depthData);
                        ++depthData;
                    }
                }
            }

            void DepthUtility::filterZeroValuesAndJumps(cv::Mat& matDepth, cv::Mat& matDepthPrevious, cv::Mat& matDepthAvg, cv::Mat& matDepthVel, float maxDepthJumpPercent)
            {
                int width = matDepth.cols;
                int height = matDepth.rows;

                for (int y = 0; y < height; ++y)
                {
                    float* resizedRow = matDepth.ptr<float>(y);
                    float* prevRow = matDepthPrevious.ptr<float>(y);
                    float* avgRow = matDepthAvg.ptr<float>(y);
                    float* velRow = matDepthVel.ptr<float>(y);

                    for (int x = 0; x < width; ++x)
                    {
                        float depth = *resizedRow;
                        float previousDepth = *prevRow;

                        //calculate percent change since last frame
                        float deltaPercent = abs(depth - previousDepth) / previousDepth;
                        //if this frame or last frame are zero depth, or there is a large jump
                        if (0 == depth || 0 == previousDepth || (deltaPercent > maxDepthJumpPercent && deltaPercent > 0))
                        {
                            //set the average to the current depth, and set velocity to zero
                            //this suppresses the velocity signal for edge jumping artifacts
                            avgRow[x] = depth;
                            velRow[x] = 0;
                        }

                        *prevRow = depth;

                        ++resizedRow;
                        ++prevRow;
                    }
                }
            }

            void DepthUtility::thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor)
            {
                int width = matForeground.cols;
                int height = matForeground.rows;

                for (int y = 0; y < height; ++y)
                {
                    float* velRow = matVelocity.ptr<float>(y);
                    char* foregroundRow = matForeground.ptr<char>(y);

                    for (int x = 0; x < width; ++x)
                    {
                        float vel = *velRow;
                        if (vel > foregroundThresholdFactor)
                        {
                            *foregroundRow = PixelType::Foreground;
                        }
                        else
                        {
                            *foregroundRow = PixelType::Background;
                        }

                        ++foregroundRow;
                        ++velRow;
                    }
                }
            }
        }
    }
}