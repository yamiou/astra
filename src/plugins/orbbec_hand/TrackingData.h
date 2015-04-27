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
        cv::Mat& score;
        cv::Mat& foreground;
        cv::Mat& layerSegmentation;
        cv::Mat& debugSegmentation;
        cv::Mat& debugSearched;
        int layerCount;

        TrackingMatrices(cv::Mat& depth,
                         cv::Mat& area,
                         cv::Mat& score,
                         cv::Mat& foreground,
                         cv::Mat& layerSegmentation,
                         cv::Mat& debugSegmentation,
                         cv::Mat& debugSearched)
            :
            depth(depth),
            area(area),
            score(score),
            foreground(foreground),
            debugSegmentation(debugSegmentation),
            layerSegmentation(layerSegmentation),
            debugSearched(debugSearched),
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