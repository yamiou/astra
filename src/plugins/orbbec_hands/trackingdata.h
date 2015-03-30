#ifndef TRACKINGDATA_H
#define TRACKINGDATA_H
#include <opencv2/core/core.hpp>

enum PixelType
{
    Background = 0,
    Foreground = 1,
    Searched = 2,
    IntermediateClosest = 3,
    Closest = 4,
    Neighborhood = 5
};

enum TrackedPointType
{
    CandidatePoint,
    ActivePoint
};

enum TrackingStatus
{
    NotTracking,
    Tracking,
    Lost,
    Dead
};

struct TrackingData
{
    cv::Mat matDepth;
    cv::Mat matArea;
    cv::Mat matGlobalSegmentation;
    cv::Mat matScore;
    cv::Mat matForegroundSearched;
    cv::Mat matLayerSegmentation;
    cv::Point seedPosition;
    const float referenceDepth;
    const float bandwidthDepth;
    const TrackedPointType pointType;
    const int iterationMax;

    TrackingData(const float referenceDepth, const float bandwidthDepth, const TrackedPointType pointType, const int iterationMax) :
        referenceDepth(referenceDepth),
        bandwidthDepth(bandwidthDepth),
        pointType(pointType),
        iterationMax(iterationMax)
    {}
};

#endif // SEGMENTATIONTRACKER_H