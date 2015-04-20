#ifndef TRACKINGDATA_H
#define TRACKINGDATA_H

#include <opencv2/core/core.hpp>

namespace sensekit { namespace plugins { namespace hands {

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

    struct TrackingMatrices
    {
        cv::Mat& matDepth;
        cv::Mat& matArea;
        cv::Mat& matScore;
        cv::Mat& matForeground;
        cv::Mat& matSegmentation;
        cv::Mat& matLayerSegmentation;
        TrackingMatrices(cv::Mat& matDepth, 
                         cv::Mat& matArea, 
                         cv::Mat& matScore, 
                         cv::Mat& matForeground,
                         cv::Mat& matSegmentation,
                         cv::Mat& matLayerSegmentation)
            :
            matDepth(matDepth),
            matArea(matArea),
            matScore(matScore),
            matForeground(matForeground),
            matSegmentation(matSegmentation),
            matLayerSegmentation(matLayerSegmentation)
        {}
    };

    struct TrackingData
    {
        TrackingMatrices& matrices;
        const cv::Point& seedPosition;
        const float referenceDepth;
        const float bandwidthDepth;
        const TrackedPointType pointType;
        const int iterationMax;

        TrackingData(TrackingMatrices& matrices, const cv::Point& seedPosition, const float referenceDepth, const float bandwidthDepth, const TrackedPointType pointType, const int iterationMax) :
            matrices(matrices),
            seedPosition(seedPosition),
            referenceDepth(referenceDepth),
            bandwidthDepth(bandwidthDepth),
            pointType(pointType),
            iterationMax(iterationMax)
        {}
    };
}}}

#endif // SEGMENTATIONTRACKER_H