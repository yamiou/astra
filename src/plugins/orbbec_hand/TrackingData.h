#ifndef TRACKINGDATA_H
#define TRACKINGDATA_H

#include <opencv2/core/core.hpp>
#include "ScalingCoordinateMapper.h"

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
        const sensekit::CoordinateMapper& fullSizeMapper;

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
                         bool debugLayersEnabled,
                         const sensekit::CoordinateMapper& fullSizeMapper)
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
            layerCount(0),
            fullSizeMapper(fullSizeMapper)
            {}
    };

    inline float get_resize_factor(TrackingMatrices& matrices)
    {
        float resizeFactor = matrices.depthFullSize.cols / static_cast<float>(matrices.depth.cols);

        return resizeFactor;
    }

    inline ScalingCoordinateMapper get_scaling_mapper(TrackingMatrices& matrices)
    {
        const float resizeFactor = get_resize_factor(matrices);

        return ScalingCoordinateMapper(matrices.fullSizeMapper, resizeFactor);
    }

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
        const float pointInertiaFactor;
        const float pointInertiaRadius;

        TrackingData(TrackingMatrices& matrices,
                     const cv::Point& seedPosition,
                     const float referenceDepth,
                     const float bandwidthDepth,
                     const TrackedPointType pointType,
                     const int iterationMax,
                     const float maxSegmentationDist,
                     const SegmentationForegroundPolicy foregroundPolicy,
                     const float edgeDistanceFactor,
                     const float targetEdgeDistance,
                     const float pointInertiaFactor,
                     const float pointInertiaRadius)
            : matrices(matrices),
              seedPosition(seedPosition),
              referenceDepth(referenceDepth),
              bandwidthDepth(bandwidthDepth),
              pointType(pointType),
              iterationMax(iterationMax),
              maxSegmentationDist(maxSegmentationDist),
              foregroundPolicy(foregroundPolicy),
              edgeDistanceFactor(edgeDistanceFactor),
              targetEdgeDistance(targetEdgeDistance),
              pointInertiaFactor(pointInertiaFactor),
              pointInertiaRadius(pointInertiaRadius)
        {}
    };
}}}

#endif // SEGMENTATIONTRACKER_H