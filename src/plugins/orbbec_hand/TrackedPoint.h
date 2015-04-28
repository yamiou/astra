#ifndef TRACKEDPOINT_H
#define TRACKEDPOINT_H

#include <opencv2/core/affine.hpp>
#include "TrackingData.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackedPoint
    {
    public:
        cv::Point position;
        cv::Point fullSizePosition;
        cv::Point3f worldPosition;
        cv::Point3f worldDeltaPosition;
        cv::Point3f steadyWorldPosition;
        int trackingId;
        int inactiveFrameCount;
        int activeFrameCount;
        TrackedPointType pointType;
        TrackingStatus trackingStatus;

        TrackedPoint(cv::Point position, cv::Point3f worldPosition, int trackingId) :
            position(position),
            fullSizePosition(),
            worldPosition(worldPosition),
            worldDeltaPosition(),
            steadyWorldPosition(worldPosition),
            trackingId(trackingId),
            inactiveFrameCount(0),
            activeFrameCount(0),
            pointType(TrackedPointType::CandidatePoint),
            trackingStatus(TrackingStatus::NotTracking)
        { }
    };
}}}

#endif // TRACKEDPOINT_H