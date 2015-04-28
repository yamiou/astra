#ifndef TRACKINGDATA_H
#define TRACKINGDATA_H

#include <opencv2/core/core.hpp>

namespace sensekit { namespace plugins { namespace hand {

    enum PixelType
    {
        Background = 0,
        Foreground = 1,
        Searched = 2
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

    enum SegmentationForegroundPolicy
    {
        FG_POLICY_IGNORE = 0,
        FG_POLICY_RESET_TTL = 1
    };

    struct TrackingMatrices
    {
        cv::Mat& depth;
        cv::Mat& area;
        cv::Mat& areaSqrt;
        cv::Mat& basicScore;
        cv::Mat& foreground;
        cv::Mat& layerSegmentation;
        cv::Mat& layerScore;
        cv::Mat& layerEdgeDistance;
        cv::Mat& foregroundSearched;
        cv::Mat& debugSegmentation;
        int layerCount;
        bool debugLayersEnabled;

        TrackingMatrices(cv::Mat& depth,
                         cv::Mat& area,
                         cv::Mat& areaSqrt,
                         cv::Mat& basicScore,
                         cv::Mat& foreground,
                         cv::Mat& foregroundSearched,
                         cv::Mat& layerSegmentation,
                         cv::Mat& layerScore,
                         cv::Mat& layerEdgeDistance,
                         cv::Mat& debugSegmentation,
                         bool debugLayersEnabled)
            :
            depth(depth),
            area(area),
            areaSqrt(areaSqrt),
            basicScore(basicScore),
            foreground(foreground),
            layerSegmentation(layerSegmentation),
            layerScore(layerScore),
            layerEdgeDistance(layerEdgeDistance),
            foregroundSearched(foregroundSearched),
            debugSegmentation(debugSegmentation),
            layerCount(0),
            debugLayersEnabled(debugLayersEnabled)
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
        const float maxSegmentationDist;
        const SegmentationForegroundPolicy foregroundPolicy;

        TrackingData(TrackingMatrices& matrices,
                     const cv::Point& seedPosition,
                     const float referenceDepth,
                     const float bandwidthDepth,
                     const TrackedPointType pointType,
                     const int iterationMax,
                     const float maxSegmentationDist,
                     const SegmentationForegroundPolicy foregroundPolicy)
            : matrices(matrices),
              seedPosition(seedPosition),
              referenceDepth(referenceDepth),
              bandwidthDepth(bandwidthDepth),
              pointType(pointType),
              iterationMax(iterationMax),
              maxSegmentationDist(maxSegmentationDist),
              foregroundPolicy(foregroundPolicy)
        {}
    };
}}}

#endif // SEGMENTATIONTRACKER_H