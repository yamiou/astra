#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H

#include <opencv2/core/affine.hpp>
#include "TrackingData.h"
#include "ScalingCoordinateMapper.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackedPoint;

    class PointProcessor
    {
    public:
        PointProcessor(const ScalingCoordinateMapper& mapper);
        virtual ~PointProcessor();

        void updateTrackedPoints(TrackingMatrices& matrices);

        void removeOldOrDeadPoints();
        void removeDuplicatePoints();
        void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                const cv::Point& seedPosition);

        std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

        void reset();

        float get_trackingBandwidthDepth() { return m_trackingBandwidthDepth; }

        float get_point_area(TrackingMatrices& matrices, const cv::Point& point);
    private:

        cv::Point adjustPointForEdge(TrackingMatrices& matrices, const cv::Point& rawTargetPoint);
        void updateTrackedPoint(TrackingMatrices& matrices, TrackedPoint& trackedPoint);
        void validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                           TrackedPoint& tracked,
                                           const cv::Point& targetPoint);
        bool is_valid_point_area(TrackingMatrices& matrices, const cv::Point& targetPoint);

        const ScalingCoordinateMapper& m_mapper;
        float m_trackingBandwidthDepth;
        float m_initialBandwidthDepth;
        float m_maxMatchDistLostActive;
        float m_maxMatchDistDefault;
        int m_iterationMaxInitial;
        int m_iterationMaxTracking;
        float m_minArea;
        float m_maxArea;
        float m_areaBandwidth;
        float m_areaBandwidthDepth;
        float m_maxSegmentationDist;
        float m_steadyDeadBandRadius;
        float m_maxJumpDist;
        float m_targetEdgeDistance;
        float m_edgeDistanceFactor;
        int m_maxInactiveFramesToBeConsideredActive;
        int m_minActiveFramesToLockTracking;
        int m_maxInactiveFramesForCandidatePoints;
        int m_maxInactiveFramesForLostPoints;
        int m_maxInactiveFramesForActivePoints;

        int m_nextTrackingId{ 0 };
        //TODO consider std::list<TrackedPoint>
        std::vector<TrackedPoint> m_trackedPoints;

    };

}}}

#endif // POINTPROCESSOR_H