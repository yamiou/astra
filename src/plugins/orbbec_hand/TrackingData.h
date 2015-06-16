#ifndef TRACKINGDATA_H
#define TRACKINGDATA_H

#include <opencv2/core/core.hpp>
#include "ScalingCoordinateMapper.h"
#include "HandSettings.h"

namespace sensekit { namespace plugins { namespace hand {

    enum PixelType
    {
        Background = 0,
        Foreground = 1,
        Searched = 2,
        SearchedFromOutOfRange = 3,
        ForegroundFailedTest = 4
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

    enum TestBehavior
    {
        TEST_BEHAVIOR_NONE = 0,
        TEST_BEHAVIOR_LOG = 1
    };

    enum TestPhase
    {
        TEST_PHASE_CREATE = 0,
        TEST_PHASE_UPDATE = 1
    };

    inline std::string tracking_status_to_string(TrackingStatus status)
    {
        switch (status)
        {
        case TrackingStatus::NotTracking:
            return "NotTracking";
        case TrackingStatus::Tracking:
            return "Tracking";
        case TrackingStatus::Lost:
            return "Lost";
        case TrackingStatus::Dead:
            return "Dead";
        default:
            return "Unknown";
        }
    }

    enum SegmentationVelocityPolicy
    {
        VELOCITY_POLICY_IGNORE = 0,
        VELOCITY_POLICY_RESET_TTL = 1
    };

    struct TrackingMatrices
    {
        cv::Mat& depthFullSize;
        cv::Mat& depth;
        cv::Mat& area;
        cv::Mat& areaSqrt;
        cv::Mat& velocitySignal;
        cv::Mat& layerSegmentation;
        cv::Mat& layerScore;
        cv::Mat& layerEdgeDistance;
        cv::Mat& layerIntegralArea;
        cv::Mat& foregroundSearched;
        cv::Mat& debugSegmentation;
        cv::Mat& debugScore;
        cv::Mat& debugScoreValue;
        cv::Mat& debugTestPassMap;
        bool enableTestPassMap;
        const sensekit::Vector3f* fullSizeWorldPoints;
        sensekit::Vector3f* worldPoints;
        bool debugLayersEnabled;
        int layerCount;
        const sensekit::CoordinateMapper& fullSizeMapper;
        const conversion_cache_t depthToWorldData;
        std::vector<sensekit::Vector2i> layerCirclePoints;

        TrackingMatrices(cv::Mat& depthFullSize,
                         cv::Mat& depth,
                         cv::Mat& area,
                         cv::Mat& areaSqrt,
                         cv::Mat& velocitySignal,
                         cv::Mat& foregroundSearched,
                         cv::Mat& layerSegmentation,
                         cv::Mat& layerScore,
                         cv::Mat& layerEdgeDistance,
                         cv::Mat& layerIntegralArea,
                         cv::Mat& debugSegmentation,
                         cv::Mat& debugScore,
                         cv::Mat& debugScoreValue,
                         cv::Mat& debugTestPassMap,
                         bool enableTestPassMap,
                         const sensekit::Vector3f* fullSizeWorldPoints,
                         sensekit::Vector3f* worldPoints,
                         bool debugLayersEnabled,
                         const sensekit::CoordinateMapper& fullSizeMapper,
                         const conversion_cache_t depthToWorldData)
            :
            depthFullSize(depthFullSize),
            depth(depth),
            area(area),
            areaSqrt(areaSqrt),
            velocitySignal(velocitySignal),
            layerSegmentation(layerSegmentation),
            layerScore(layerScore),
            layerEdgeDistance(layerEdgeDistance),
            layerIntegralArea(layerIntegralArea),
            foregroundSearched(foregroundSearched),
            debugSegmentation(debugSegmentation),
            debugScore(debugScore),
            debugScoreValue(debugScoreValue),
            debugTestPassMap(debugTestPassMap),
            enableTestPassMap(enableTestPassMap),
            fullSizeWorldPoints(fullSizeWorldPoints),
            worldPoints(worldPoints),
            debugLayersEnabled(debugLayersEnabled),
            layerCount(0),
            fullSizeMapper(fullSizeMapper),
            depthToWorldData(depthToWorldData)
            { }
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
        const cv::Point3f referenceWorldPosition;
        const float referenceAreaSqrt;
        const SegmentationVelocityPolicy velocityPolicy;
        const SegmentationSettings settings;
        const TestPhase phase;

        TrackingData(TrackingMatrices& matrices,
                     const cv::Point& seedPosition,
                     const cv::Point3f referenceWorldPosition,
                     const float referenceAreaSqrt,
                     const SegmentationVelocityPolicy velocityPolicy,
                     const SegmentationSettings settings,
                     const TestPhase phase)
            : matrices(matrices),
              seedPosition(seedPosition),
              referenceWorldPosition(referenceWorldPosition),
              referenceAreaSqrt(referenceAreaSqrt),
              velocityPolicy(velocityPolicy),
              settings(settings),
              phase(phase)
        {}
    };
}}}

#endif // SEGMENTATIONTRACKER_H
