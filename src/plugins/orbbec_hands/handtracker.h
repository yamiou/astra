#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL.h>

#include "depthutility.h"
#include "trackedpoint.h"
#include "pointprocessor.h"
#include "../../SenseKitUL/SenseKitUL_internal.h"
#include "coordinateconverter.h"

class HandTracker
{
public:
    HandTracker(sensekit_depthstream_t* depthStream);
    virtual ~HandTracker();
    void reset();
private:

    static void copyPosition(cv::Point3f& source, sensekit_vector3f_t& target);
    static sensekit_handstatus_t convertHandStatus(TrackingStatus status);
    static void resetHandPoint(sensekit_handpoint_t& point);

    void updateTracking(sensekit_depthframe_t* depthFrame);
    void updateHandFrame(std::vector<TrackedPoint>& internalTrackedPoints, sensekit_handframe_wrapper_t* handframe_wrapper);

    void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);
    void setupVariables();
    
    //fields    
    sensekit_depthstream_t* m_depthStream;
    DepthUtility m_depthUtility;
    CoordinateConverter m_converter;
    PointProcessor m_pointProcessor;

    int			m_width;
    int			m_height;
    
    float m_resizeFactor;

    cv::Mat m_matDepth;
    cv::Mat m_matForeground;
    
    float m_depthSmoothingFactor;
    float m_foregroundThresholdFactor;
    float m_maxDepthJumpPercent;

    sensekit_handframe_wrapper_t* m_wrapper { nullptr };
    
    float m_factor1;
    float m_factor2;
};


#endif // HANDTRACKER_H
