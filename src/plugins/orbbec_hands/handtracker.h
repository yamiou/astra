#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL.h>

#include "depthutility.h"
#include "trackedpoint.h"
#include "pointprocessor.h"


class HandTracker
{
public:
    HandTracker(sensekit_depthstream_t* depthStream);
    virtual ~HandTracker();
    void onKey(unsigned char key);
    std::vector<TrackedPoint>& updateTracking(sensekit_depthframe_t* depthFrame);
    void reset();
private:

    DepthUtility m_depthUtility;
    PointProcessor m_pointProcessor;

    std::vector<TrackedPoint>& updateOriginalPoints(std::vector<TrackedPoint>& mInternalTrackedPoints);

    static void removeDuplicatePoints(std::vector<TrackedPoint>& trackedPoints);
    static void removeOldOrDeadPoints(std::vector<TrackedPoint>& trackedPoints);
    
    void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);
    void setupVariables();
    void verifyInit(int width, int height);

    //fields    
    sensekit_depthstream_t* m_depthStream;

    int			m_width;
    int			m_height;
    float m_maxVelocity;

    float m_resizeFactor;

    std::vector<TrackedPoint> m_internalTrackedPoints;
    std::vector<TrackedPoint> m_originalTrackedPoints;

    cv::Mat m_matDepth;
    cv::Mat m_matForeground;
    
    bool m_isInitialized{ false };

    float m_velocitySmoothingFactor;
    float m_depthSmoothingFactor;
    float m_maxVelocityFactor;
    float m_foregroundThresholdFactor;
    
    int m_nextTrackingId;

    float m_maxDepthJumpPercent;
    bool m_outputSample;
    bool m_showCircles;
    bool m_showForeground;
    float m_minArea;
    float m_maxArea;
    float m_areaBandwidth;
    float m_areaBandwidthDepth;

    long double m_calcDuration, m_frameDuration;
    double m_fpsFactor;

    float m_factor1;
    float m_factor2;
    float m_factor3;
    float m_factor4;

    int mouseX;
    int mouseY;
};


#endif // HANDTRACKER_H
