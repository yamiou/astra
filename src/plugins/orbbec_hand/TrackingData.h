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
        cv::Mat& depthFullSize;
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
        cv::Mat& debugScore;
        bool debugLayersEnabled;
        int layerCount;

        TrackingMatrices(cv::Mat& depthFullSize,
                         cv::Mat& depth,
                         cv::Mat& area,
                         cv::Mat& areaSqrt,
                         cv::Mat& basicScore,
                         cv::Mat& foreground,
                         cv::Mat& foregroundSearched,
                         cv::Mat& layerSegmentation,
                         cv::Mat& layerScore,
                         cv::Mat& layerEdgeDistance,
                         cv::Mat& debugSegmentation,
                         cv::Mat& debugScore,
                         bool debugLayersEnabled)
            :
            depthFullSize(depthFullSize),
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
            debugScore(debugScore),
            debugLayersEnabled(debugLayersEnabled),
            layerCount(0)
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
        const float edgeDistanceFactor;
        const float targetEdgeDistance;

        TrackingData(TrackingMatrices& matrices,
                     const cv::Point& seedPosition,
                     const float referenceDepth,
                     const float bandwidthDepth,
                     const TrackedPointType pointType,
                     const int iterationMax,
                     const float maxSegmentationDist,
                     const SegmentationForegroundPolicy foregroundPolicy,
                     const float edgeDistanceFactor,
                     const float targetEdgeDistance)
            : matrices(matrices),
              seedPosition(seedPosition),
              referenceDepth(referenceDepth),
              bandwidthDepth(bandwidthDepth),
              pointType(pointType),
              iterationMax(iterationMax),
              maxSegmentationDist(maxSegmentationDist),
              foregroundPolicy(foregroundPolicy),
              edgeDistanceFactor(edgeDistanceFactor),
              targetEdgeDistance(targetEdgeDistance)
        {}
    };
}}}

#endif // SEGMENTATIONTRACKER_H