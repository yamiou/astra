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

    void updateTrackedPoint(TrackingMatrices matrices, TrackedPoint& trackedPoint);

    void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices matrices, std::vector<TrackedPoint>& trackedPoints, cv::Point seedPosition, int& nextTrackingId);



private:
    void validateAndUpdateTrackedPoint(TrackingMatrices matrices, TrackedPoint& tracked, cv::Point targetPoint);

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

};

#endif // POINTPROCESSOR_H