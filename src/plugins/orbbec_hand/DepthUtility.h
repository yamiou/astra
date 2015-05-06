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

        void processDepthToVelocitySignal(DepthFrame& depthFrame,
                                          cv::Mat& matDepth,
                                          cv::Mat& matDepthFullSize,
                                          cv::Mat& matVelocitySignal);
        void reset();

        const cv::Mat& matDepthVel() const { return m_matDepthVel; }
        const cv::Mat& matDepthAvg() const { return m_matDepthAvg; }
        const cv::Mat& matDepthVelErode() const { return m_matDepthVelErode; }
        const cv::Mat& matDepthFilled() const { return m_matDepthFilled; }

    private:

        enum FillMaskType
        {
            Normal = 0,
            Filled = 1
        };
        static void depthFrameToMat(DepthFrame& depthFrameSrc, 
                                    const int width, 
                                    const int height, 
                                    const float maxDepth, 
                                    cv::Mat& matTarget,
                                    const float farDepth);

        static void fillZeroValues(cv::Mat& matDepth,
                                   cv::Mat& matDepthFilled,
                                   cv::Mat& matDepthFilledMask,
                                   cv::Mat& matDepthPrevious);

        static void filterZeroValuesAndJumps(cv::Mat& depthCurrent,
                                             cv::Mat& depthPrev,
                                             cv::Mat& depthAvg,
                                             cv::Mat& matDepthFilledMask,
                                             const float maxDepthJumpPercent);

        void thresholdVelocitySignal(cv::Mat& matVelocitySignal,
                                     cv::Mat& matVelocityFiltered,
                                     const float velocityThresholdFactor);

        const float m_processingWidth;
        const float m_processingHeight;

        cv::Mat m_rectElement;
        cv::Mat m_rectElement2;
        cv::Mat m_matDepthOriginal;
        cv::Mat m_matDepthPrevious;
        cv::Mat m_matDepthFilled;
        cv::Mat m_matDepthFilledMask;
        cv::Mat m_matDepthAvg;
        cv::Mat m_matDepthVel;
        cv::Mat m_matDepthVelErode;

        float m_depthSmoothingFactor;
        float m_velocityThresholdFactor;
        float m_maxDepthJumpPercent;
        int m_erodeSize;
        float m_maxVel;
        float m_maxDepth;
        float m_farDepth;
    };
}}}

#endif // DEPTHUTILITY_H