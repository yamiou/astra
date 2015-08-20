#ifndef DEPTHUTILITY_H
#define DEPTHUTILITY_H

#include <opencv2/imgproc/imgproc.hpp>
#include <AstraUL/AstraUL.h>
#include "HandSettings.h"

namespace astra { namespace plugins { namespace hand {

    class DepthUtility
    {
    public:
        DepthUtility(float width, float height, DepthUtilitySettings& settings);
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
                                    cv::Mat& matTarget);

        static void fillZeroValues(cv::Mat& matDepth,
                                   cv::Mat& matDepthFilled,
                                   cv::Mat& matDepthFilledMask,
                                   cv::Mat& matDepthPrevious);

        static void filterZeroValuesAndJumps(cv::Mat& depthCurrent,
                                             cv::Mat& depthPrev,
                                             cv::Mat& depthAvg,
                                             cv::Mat& matDepthFilledMask,
                                             const float maxDepthJumpPercent);
        void thresholdVelocitySignal(cv::Mat& matVelocityFiltered,
                                     cv::Mat& matVelocitySignal,
                                     const float velocityThresholdFactor);

        void adjust_velocities_for_depth(cv::Mat& matDepth,
                                         cv::Mat& matVelocityFiltered);

        int depth_to_chunk_index(float depth);

        void analyze_velocities(cv::Mat& matDepth,
                                cv::Mat& matVelocityFiltered);

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
        float m_depthAdjustmentFactor;
        float m_minDepth;
        float m_maxDepth;

        static const int NUM_DEPTH_VEL_CHUNKS = 8;
        const float MIN_CHUNK_DEPTH = 0;
        const float MAX_CHUNK_DEPTH = 8000;
        float m_velErodeFactor{ 0.98 };
        float m_maxVel[NUM_DEPTH_VEL_CHUNKS];
        float m_depthCount[NUM_DEPTH_VEL_CHUNKS];
    };
}}}

#endif // DEPTHUTILITY_H
