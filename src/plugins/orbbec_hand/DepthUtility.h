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
        const cv::Mat& matDepthMod() const { return m_matDepthFilled; }

    private:
        static void depthFrameToMat(DepthFrame& depthFrameSrc, 
                                    const int width, 
                                    const int height, 
                                    const float maxDepth, 
                                    cv::Mat& matTarget,
                                    const float farDepth);

        static void fillZeroValues(cv::Mat& matDepth, cv::Mat& matDepthFilled, cv::Mat& matDepthPrevious, const float invalidDepth);

        static void filterZeroValuesAndJumps(cv::Mat& depthCurrent,
                                             cv::Mat& depthPrev,
                                             cv::Mat& depthAvg,
                                             cv::Mat& depthVel,
                                             const float maxDepthJumpPercent,
                                             const float invalidDepth);

        void thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor);

        const float m_processingWidth;
        const float m_processingHeight;

        cv::Mat m_rectElement;
        cv::Mat m_rectElement2;
        cv::Mat m_matDepthOriginal;
        cv::Mat m_matDepthPrevious;
        cv::Mat m_matDepthFilled;
        cv::Mat m_matDepthAvg;
        cv::Mat m_matDepthVel;
        cv::Mat m_matDepthVelErode;

        float m_depthSmoothingFactor;
        float m_foregroundThresholdFactor;
        float m_maxDepthJumpPercent;
        int m_erodeSize;
        float m_maxVel;
        float m_maxDepth;
        float m_farDepth;
    };
}}}

#endif // DEPTHUTILITY_H