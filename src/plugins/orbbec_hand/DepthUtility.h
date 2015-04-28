#ifndef DEPTHUTILITY_H
#define DEPTHUTILITY_H

#include <opencv2/opencv.hpp>
#include <SenseKitUL/SenseKitUL.h>

namespace sensekit { namespace plugins { namespace hand {

    class DepthUtility
    {
    public:
        DepthUtility(int width, int height);
        virtual ~DepthUtility();

        void processDepthToForeground(DepthFrame& depthFrame, 
                                      cv::Mat& matDepth,
                                      cv::Mat& matDepthFullSize, 
                                      cv::Mat& matForeground);
        void reset();

        const cv::Mat& matDepthVel() const { return m_matDepthVel; }
        const cv::Mat& matDepthVelErode() const { return m_matDepthVelErode; }

    private:
        static void depthFrameToMat(DepthFrame& depthFrameSrc, int width, int height, cv::Mat& matTarget);

        static void filterZeroValuesAndJumps(cv::Mat& depthCurrent,
                                             cv::Mat& depthPrev,
                                             cv::Mat& depthAvg,
                                             cv::Mat& depthVel,
                                             float maxDepthJumpPercent);

        static void thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor);

        const float m_processingWidth;
        const float m_processingHeight;

        cv::Mat m_rectElement;
        cv::Mat m_matDepthOriginal;
        cv::Mat m_matDepthPrevious;
        cv::Mat m_matDepthAvg;
        cv::Mat m_matDepthVel;
        cv::Mat m_matDepthVelErode;

        float m_depthSmoothingFactor;
        float m_foregroundThresholdFactor;
        float m_maxDepthJumpPercent;
    };
}}}

#endif // DEPTHUTILITY_H