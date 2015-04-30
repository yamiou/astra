#include "DepthUtility.h"
#include <SenseKitUL/SenseKitUL.h>
#include "TrackingData.h"
#include <cmath>

namespace sensekit { namespace plugins { namespace hand {

    DepthUtility::DepthUtility(int width, int height) :
        m_processingWidth(width),
        m_processingHeight(height),
        m_depthSmoothingFactor(0.05),
        m_foregroundThresholdFactor(0.02),
        m_maxDepthJumpPercent(0.1),
        m_erodeSize(1),
        m_maxVel(0),
        m_maxDepth(4000),
        m_farDepth(10000)
    {
        m_rectElement = cv::getStructuringElement(cv::MORPH_RECT,
                                                  cv::Size(m_erodeSize * 2 + 1, m_erodeSize * 2 + 1),
                                                  cv::Point(m_erodeSize, m_erodeSize));

        reset();
    }

    DepthUtility::~DepthUtility()
    {
    }

    void DepthUtility::reset()
    {
        m_matDepthFilled = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthPrevious = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthAvg = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthVel.create(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthVelErode.create(m_processingHeight, m_processingWidth, CV_32FC1);
    }

    void DepthUtility::processDepthToForeground(DepthFrame& depthFrame,
                                                cv::Mat& matDepth,
                                                cv::Mat& matDepthFullSize,
                                                cv::Mat& matForeground)
    {
        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();

        matDepth.create(m_processingHeight, m_processingWidth, CV_32FC1);
        matForeground = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_8UC1);

        depthFrameToMat(depthFrame, width, height, m_maxDepth, matDepthFullSize, m_farDepth);

        //convert to the target processing size with nearest neighbor

        cv::resize(matDepthFullSize, matDepth, matDepth.size(), 0, 0, CV_INTER_NN);

        //fill 0 depth pixels with the value from the previous frame
        fillZeroValues(matDepth, m_matDepthFilled, m_matDepthPrevious, m_farDepth);

        //current minus average, scaled by average = velocity as a percent change

        m_matDepthVel = (m_matDepthFilled - m_matDepthAvg) / m_matDepthAvg;

        //accumulate current frame to average using smoothing factor

        cv::accumulateWeighted(m_matDepthFilled, m_matDepthAvg, m_depthSmoothingFactor);

        filterZeroValuesAndJumps(m_matDepthFilled, 
                                 m_matDepthPrevious, 
                                 m_matDepthAvg, 
                                 m_matDepthVel, 
                                 m_maxDepthJumpPercent, 
                                 m_farDepth);

        //erode to eliminate single pixel velocity artifacts
        m_matDepthVelErode = cv::abs(m_matDepthVel);
        cv::erode(m_matDepthVelErode, m_matDepthVelErode, m_rectElement);
        cv::dilate(m_matDepthVelErode, m_matDepthVelErode, m_rectElement);

        thresholdForeground(matForeground, m_matDepthVelErode, m_foregroundThresholdFactor);
    }

    void DepthUtility::depthFrameToMat(DepthFrame& depthFrameSrc, 
                                       const int width, 
                                       const int height, 
                                       const float maxDepth,
                                       cv::Mat& matTarget,
                                       const float farDepth)
    {
        //ensure initialized
        matTarget.create(height, width, CV_32FC1);

        const int16_t* depthData = depthFrameSrc.data();

        for (int y = 0; y < height; ++y)
        {
            float* row = matTarget.ptr<float>(y);
            for (int x = 0; x < width; ++x)
            {
                float depth = static_cast<float>(*depthData);
                if (depth > maxDepth)
                {
                    depth = farDepth;
                }
                *row = depth;
                ++row;
                ++depthData;
            }
        }
    }

    void DepthUtility::fillZeroValues(cv::Mat& matDepth, cv::Mat& matDepthFilled, cv::Mat& matDepthPrevious, const float invalidDepth)
    {
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* filledDepthRow = matDepthFilled.ptr<float>(y);

            for (int x = 0; x < width; ++x)
            {
                float depth = *depthRow;

                if (depth == 0)
                {
                    depth = *prevDepthRow;
                }

                *filledDepthRow = depth;

                ++depthRow;
                ++prevDepthRow;
                ++filledDepthRow;
            }
        }
    }

    void DepthUtility::filterZeroValuesAndJumps(cv::Mat& matDepth,
                                                cv::Mat& matDepthPrevious,
                                                cv::Mat& matDepthAvg,
                                                cv::Mat& matDepthVel,
                                                const float maxDepthJumpPercent,
                                                const float invalidDepth)
    {
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* avgRow = matDepthAvg.ptr<float>(y);
            float* velRow = matDepthVel.ptr<float>(y);

            for (int x = 0; x < width; ++x)
            {
                float depth = *depthRow;
                float previousDepth = *prevDepthRow;

                //calculate percent change since last frame
                float deltaPercent = (depth - previousDepth) / previousDepth;
                float absDeltaPercent = std::fabs(deltaPercent);
                //if this frame or last frame are zero depth, or there is a large jump

                if (0 == depth || 0 == previousDepth ||
                    (absDeltaPercent > maxDepthJumpPercent && deltaPercent > 0))
                {
                    //set the average to the current depth, and set velocity to zero
                    //this suppresses the velocity signal for edge jumping artifacts
                    avgRow[x] = depth;
                    velRow[x] = 0;
                }

                *prevDepthRow = depth;

                ++depthRow;
                ++prevDepthRow;
            }
        }
    }

    void DepthUtility::thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor)
    {
        int width = matForeground.cols;
        int height = matForeground.rows;

        m_maxVel *= 0.98;
        for (int y = 0; y < height; ++y)
        {
            float* velRow = matVelocity.ptr<float>(y);
            char* foregroundRow = matForeground.ptr<char>(y);

            for (int x = 0; x < width; ++x)
            {
                //matVelocity is already abs(vel)
                float vel = *velRow;
                if (vel > m_maxVel)
                {
                    m_maxVel = vel;
                }
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
        //printf("max vel: %f\n", m_maxVel);
    }

}}}