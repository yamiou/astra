#ifndef DEPTHUTILITY_H
#define DEPTHUTILITY_H

class DepthUtility
{
public:
    DepthUtility(int width, int height);
    virtual ~DepthUtility();

    void processDepthToForeground(sensekit_depthframe_t* depthFrame, int width, int height, cv::Mat matDepth, cv::Mat matForeground);
    void reset();

private:
    static void depthFrameToMat(sensekit_depthframe_t* depthFrameSrc, int width, int height, cv::Mat matTarget);

    static void filterZeroValuesAndJumps(cv::Mat depthCurrent, cv::Mat depthPrev, cv::Mat depthAvg, cv::Mat depthVel, float maxDepthJumpPercent);
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

#endif // DEPTHUTILITY_H