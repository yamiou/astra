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
        PointProcessor(const sensekit::CoordinateMapper& mapper);
        virtual ~PointProcessor();

        void initialize_common_calculations(TrackingMatrices& matrices);
        void updateTrackedPoints(TrackingMatrices& matrices);

        void removeDuplicatePoints();
        void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                const cv::Point& seedPosition);
        void removeOldOrDeadPoints();
        void refine_active_points(TrackingMatrices& matrices);

        std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

        void reset();

        float get_trackingBandwidthDepth() { return m_trackingBandwidthDepth; }

        float get_point_area(TrackingMatrices& matrices, const cv::Point& point);
    private:
        float get_resize_factor(TrackingMatrices& matrices);
        ScalingCoordinateMapper get_scaling_mapper(TrackingMatrices& matrices);

        void updateTrackedPoint(TrackingMatrices& matrices,
                                ScalingCoordinateMapper& scalingMapper,
                                TrackedPoint& trackedPoint);

        void refine_high_res_position(TrackingMatrices& matrices,
                                      TrackedPoint& trackedPoint);

        void validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                           ScalingCoordinateMapper& scalingMapper,
                                           TrackedPoint& trackedPoint,
                                           const cv::Point& targetPoint);
        bool is_valid_point_area(TrackingMatrices& matrices, const cv::Point& targetPoint);

        const sensekit::CoordinateMapper& m_fullSizeMapper;
        float m_trackingBandwidthDepth;
        float m_initialBandwidthDepth;
        float m_maxMatchDistLostActive;
        float m_maxMatchDistDefault;
        int m_iterationMaxInitial;
        int m_iterationMaxTracking;
        int m_iterationMaxRefinement;
        float m_minArea;
        float m_maxArea;
        float m_areaBandwidth;
        float m_areaBandwidthDepth;
        float m_maxSegmentationDist;
        float m_steadyDeadBandRadius;
        float m_maxJumpDist;
        float m_targetEdgeDistance;
        float m_heightScoreFactor;
        float m_depthScoreFactor;
        float m_edgeDistanceScoreFactor;
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