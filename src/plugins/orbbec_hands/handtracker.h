#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL.h>

#include "depthutility.h"
#include "trackedpoint.h"
#include "pointprocessor.h"
#include "../../SenseKitUL/SenseKitUL_internal.h"


class HandTracker
{
public:
    HandTracker(sensekit_depthstream_t* depthStream);
    virtual ~HandTracker();
    void onKey(unsigned char key);
    void reset();
private:

    DepthUtility m_depthUtility;
    PointProcessor m_pointProcessor;
    
    static void copyPosition(cv::Point3f& source, sensekit_vector3f_t& target);
    static sensekit_handstatus_t convertHandStatus(TrackingStatus status);
    static void resetHandPoint(sensekit_handpoint_t& point);

    void updateTracking(sensekit_depthframe_t* depthFrame);
    void updateHandFrame(std::vector<TrackedPoint>& internalTrackedPoints, sensekit_handframe_wrapper_t* handframe_wrapper);

    void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);
    void setupVariables();
    void verifyInit(int width, int height);

    //fields    
    sensekit_depthstream_t* m_depthStream;

    int			m_width;
    int			m_height;
    float m_maxVelocity;

    float m_resizeFactor;

    cv::Mat m_matDepth;
    cv::Mat m_matForeground;
    
    bool m_isInitialized{ false };

    float m_velocitySmoothingFactor;
    float m_depthSmoothingFactor;
    float m_maxVelocityFactor;
    float m_foregroundThresholdFactor;
    
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

    sensekit_handframe_wrapper_t* m_wrapper { nullptr };
    float m_factor1;
    float m_factor2;
    float m_factor3;
    float m_factor4;

    int mouseX;
    int mouseY;
};


#endif // HANDTRACKER_H
