#include "DepthUtility.h"
#include <SenseKitUL/SenseKitUL.h>
#include "TrackingData.h"
#include <cmath>

namespace sensekit { namespace plugins { namespace hand {

    DepthUtility::DepthUtility(int width, int height) :
        m_processingWidth(width),
        m_processingHeight(height),
        m_depthSmoothingFactor(0.05),
        m_velocityThresholdFactor(0.02),
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
        m_matDepthFilledMask = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_8UC1);
        m_matDepthPrevious = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthAvg = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthVel.create(m_processingHeight, m_processingWidth, CV_32FC1);
        m_matDepthVelErode.create(m_processingHeight, m_processingWidth, CV_32FC1);
    }

    void DepthUtility::processDepthToVelocitySignal(DepthFrame& depthFrame,
                                                    cv::Mat& matDepth,
                                                    cv::Mat& matDepthFullSize,
                                                    cv::Mat& matVelocitySignal)
    {
        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();

        matDepth.create(m_processingHeight, m_processingWidth, CV_32FC1);
        matVelocitySignal = cv::Mat::zeros(m_processingHeight, m_processingWidth, CV_8UC1);

        depthFrameToMat(depthFrame, width, height, m_maxDepth, matDepthFullSize, m_farDepth);

        //convert to the target processing size with nearest neighbor

        cv::resize(matDepthFullSize, matDepth, matDepth.size(), 0, 0, CV_INTER_NN);

        //fill 0 depth pixels with the value from the previous frame
        fillZeroValues(matDepth, m_matDepthFilled, m_matDepthFilledMask, m_matDepthPrevious);

        //accumulate current frame to average using smoothing factor

        cv::accumulateWeighted(m_matDepthFilled, m_matDepthAvg, m_depthSmoothingFactor);

        filterZeroValuesAndJumps(m_matDepthFilled,
                                 m_matDepthPrevious,
                                 m_matDepthAvg,
                                 m_matDepthFilledMask,
                                 m_maxDepthJumpPercent,
                                 m_farDepth);

        //current minus average, scaled by average = velocity as a percent change

        m_matDepthVel = (m_matDepthFilled - m_matDepthAvg) / m_matDepthAvg;

        //erode to eliminate single pixel velocity artifacts
        m_matDepthVelErode = cv::abs(m_matDepthVel);
        cv::erode(m_matDepthVelErode, m_matDepthVelErode, m_rectElement);
        cv::dilate(m_matDepthVelErode, m_matDepthVelErode, m_rectElement);

        thresholdVelocitySignal(matVelocitySignal,
                                m_matDepthVelErode,
                                m_velocityThresholdFactor);
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

    void DepthUtility::fillZeroValues(cv::Mat& matDepth,
                                      cv::Mat& matDepthFilled,
                                      cv::Mat& matDepthFilledMask,
                                      cv::Mat& matDepthPrevious)
    {
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* filledDepthRow = matDepthFilled.ptr<float>(y);
            uint8_t* filledDepthMaskRow = matDepthFilledMask.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x)
            {
                float depth = *depthRow;

                uint8_t fillType = *filledDepthMaskRow;

                if (depth == 0)
                {
                    depth = *prevDepthRow;
                    *filledDepthMaskRow = FillMaskType::Filled;
                }
                else
                {
                    *filledDepthMaskRow = FillMaskType::Normal;
                }

                *filledDepthRow = depth;

                ++depthRow;
                ++prevDepthRow;
                ++filledDepthRow;
                ++filledDepthMaskRow;
            }
        }
    }

    void DepthUtility::filterZeroValuesAndJumps(cv::Mat& matDepth,
                                                cv::Mat& matDepthPrevious,
                                                cv::Mat& matDepthAvg,
                                                cv::Mat& matDepthFilledMask,
                                                const float maxDepthJumpPercent,
                                                const float farDepth)
    {
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* avgRow = matDepthAvg.ptr<float>(y);
            uint8_t* filledDepthMaskRow = matDepthFilledMask.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x)
            {
                float depth = *depthRow;
                float previousDepth = *prevDepthRow;
                uint8_t fillType = *filledDepthMaskRow;

                //calculate percent change since last frame
                float deltaPercent = (depth - previousDepth) / previousDepth;
                float absDeltaPercent = std::fabs(deltaPercent);
                bool movingAway = deltaPercent > 0;

                //suppress signal if either current or previous pixel are invalid
                bool isZeroDepth = (0 == depth || 0 == previousDepth);

                //suppress signal when a pixel was artificially filled
                bool isFilled = (fillType == FillMaskType::Filled);

                //suppress signal when a pixel jumps a long distance from near to far
                bool isJumpingAway = (absDeltaPercent > maxDepthJumpPercent && movingAway);
                bool isFarDepth = (depth == farDepth || previousDepth == farDepth);

                if (isZeroDepth || isFilled || isJumpingAway || isFarDepth)
                {
                    //set the average to the current depth, and set velocity to zero
                    //this suppresses the velocity signal for edge jumping artifacts
                    avgRow[x] = depth;
                }

                *prevDepthRow = depth;

                ++depthRow;
                ++prevDepthRow;
                ++filledDepthMaskRow;
            }
        }
    }

    void DepthUtility::thresholdVelocitySignal(cv::Mat& matVelocitySignal,
                                               cv::Mat& matVelocityFiltered,
                                               const float velocityThresholdFactor)
    {
        int width = matVelocitySignal.cols;
        int height = matVelocitySignal.rows;

        m_maxVel *= 0.98;
        for (int y = 0; y < height; ++y)
        {
            float* velFilteredRow = matVelocityFiltered.ptr<float>(y);
            uint8_t* velocitySignalRow = matVelocitySignal.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x, ++velFilteredRow, ++velocitySignalRow)
            {
                //matVelocityFiltered is already abs(vel)
                float velFiltered = *velFilteredRow;
                if (velFiltered > m_maxVel)
                {
                    m_maxVel = velFiltered;
                }

                if (velFiltered > velocityThresholdFactor)
                {
                    *velocitySignalRow = PixelType::Foreground;
                }
                else
                {
                    *velocitySignalRow = PixelType::Background;
                }
            }
        }
        //printf("max vel: %f\n", m_maxVel);
    }
}}}