#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/opencv.hpp>
#include <SenseKitUL.h>
#include <string>
#include <deque>

#include "trackingdata.h"

struct TrackedPoint
{
public:
    cv::Point m_position;
    cv::Point3f m_worldPosition;
    cv::Point3f m_steadyWorldPosition;
    cv::Point3f m_worldVelocity;
    int m_trackingId;
    int m_inactiveFrameCount;
    float m_totalContributionArea;
    float m_avgArea;
    int m_wrongAreaCount;
    int m_activeFrameCount;
    TrackedPointType m_type;
    TrackingStatus m_status;

    TrackedPoint(cv::Point position, cv::Point3f worldPosition, int trackingId, float area)
    {
        m_type = TrackedPointType::CandidatePoint;
        m_status = TrackingStatus::NotTracking;
        m_position = position;
        m_worldPosition = worldPosition;
        m_steadyWorldPosition = worldPosition;
        m_worldVelocity = cv::Point3f(0, 0, 0);
        m_trackingId = trackingId;
        m_inactiveFrameCount = 0;
        m_activeFrameCount = 0;
        m_totalContributionArea = 0;
        m_avgArea = area;
        m_wrongAreaCount = 0;
    }
};

class HandTracker
{
public:
    HandTracker(sensekit_depthstream_t* depthStream);
    virtual ~HandTracker();
    void onKey(unsigned char key);
    std::vector<TrackedPoint>& updateTracking(sensekit_depthframe_t* depthFrame);
    void reset();
private:
    
    std::vector<TrackedPoint>& updateOriginalPoints(std::vector<TrackedPoint>& mInternalTrackedPoints);

    static void filterZeroValuesAndJumps(cv::Mat depthCurrent, cv::Mat depthPrev, cv::Mat depthAvg, cv::Mat depthVel, float maxDepthJumpPercent);
    static void thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor);

    static float countNeighborhoodArea(cv::Mat& matForeground, cv::Mat& matDepth, cv::Mat& matArea, cv::Point_<int> center, const float bandwidth, const float bandwidthDepth, const float resizeFactor);
    static void validateAndUpdateTrackedPoint(cv::Mat& matDepth, cv::Mat& matArea, cv::Mat& matLayerSegmentation, TrackedPoint& tracked, cv::Point targetPoint, const float resizeFactor, const float minArea, const float maxArea, const float areaBandwidth, const float areaBandwidthDepth);

    static void removeDuplicatePoints(std::vector<TrackedPoint>& trackedPoints);
    static void removeOldOrDeadPoints(std::vector<TrackedPoint>& trackedPoints);

    void trackPoints(cv::Mat& matForeground, cv::Mat& matDepth, cv::Mat& matGlobalSegmentation, cv::Mat& matScore, cv::Mat& matEdgeDistance, cv::Mat& matArea);
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

    cv::Mat m_matDepthOriginal;
    cv::Mat m_matDepth;
    cv::Mat m_matDepthPrevious;
    cv::Mat m_matDepthAvg;
    cv::Mat m_matDepthVel;
    cv::Mat m_matDepthVelErode;
    cv::Mat m_matForeground;
    cv::Mat m_matGlobalSegmentation;
    cv::Mat m_matEdgeDistance;
    cv::Mat m_matScore;
    cv::Mat m_layerSegmentation;
    cv::Mat m_matLocalArea;
    cv::Mat m_tempLocalArea;

    cv::Mat m_rectElement;
    
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
