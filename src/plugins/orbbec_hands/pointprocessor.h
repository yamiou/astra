#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H
#include <opencv2/core/affine.hpp>
#include "trackingdata.h"

class TrackedPoint;

class PointProcessor
{
public:
    PointProcessor();
    virtual ~PointProcessor();

    void updateTrackedPoints(TrackingMatrices matrices);

    void removeOldOrDeadPoints();
    void removeDuplicatePoints();
    void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices matrices, cv::Point seedPosition);

    std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

private:

    void updateTrackedPoint(TrackingMatrices matrices, TrackedPoint& trackedPoint);
    void validateAndUpdateTrackedPoint(TrackingMatrices matrices, TrackedPoint& tracked, cv::Point targetPoint);
    bool isValidPointArea(TrackingMatrices& matrices, cv::Point targetPoint);
    
    float maxMatchDistLostActive;
    float maxMatchDistDefault;
    float trackingBandwidthDepth;
    float initialBandwidthDepth;
    int iterationMaxInitial;
    int iterationMaxTracking;
    float resizeFactor;
    float minArea;
    float maxArea;
    float areaBandwidth;
    float areaBandwidthDepth;

    int m_nextTrackingId{ 0 };
    std::vector<TrackedPoint> m_trackedPoints;

};

#endif // POINTPROCESSOR_H