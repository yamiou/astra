#ifndef TRACKEDPOINT_H
#define TRACKEDPOINT_H

#include <opencv2/core/affine.hpp>
#include "TrackingData.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackedPoint
    {
    public:
        cv::Point m_position;
        cv::Point3f m_worldPosition;
        cv::Point3f m_steadyWorldPosition;
        cv::Point3f m_worldDeltaPosition;
        int m_trackingId;
        int m_inactiveFrameCount;
        float m_totalContributionArea;
        int m_wrongAreaCount;
        int m_activeFrameCount;
        TrackedPointType m_type;
        TrackingStatus m_status;

        TrackedPoint(cv::Point position, cv::Point3f worldPosition, int trackingId)
        {
            m_type = TrackedPointType::CandidatePoint;
            m_status = TrackingStatus::NotTracking;
            m_position = position;
            m_worldPosition = worldPosition;
            m_steadyWorldPosition = worldPosition;
            m_worldDeltaPosition = cv::Point3f(0, 0, 0);
            m_trackingId = trackingId;
            m_inactiveFrameCount = 0;
            m_activeFrameCount = 0;
            m_totalContributionArea = 0;
            m_wrongAreaCount = 0;
        }
    };
}}}

#endif // TRACKEDPOINT_H