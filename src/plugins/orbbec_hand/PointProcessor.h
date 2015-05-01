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
        PointProcessor();
        virtual ~PointProcessor();

        void initialize_common_calculations(TrackingMatrices& matrices);
        void updateTrackedPoints(TrackingMatrices& matrices);

        void removeDuplicatePoints();
        void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                const cv::Point& seedPosition);
        void removeOldOrDeadPoints();
        
        void update_full_resolution_points(TrackingMatrices& matrices);

        std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

        void reset();

        float get_trackingBandwidthDepth() { return m_updateCycleBandwidthDepth; }

        float get_point_area(TrackingMatrices& matrices, const cv::Point& point);
    private:
        cv::Point3f smooth_world_positions(const cv::Point3f& oldWorldPosition, const cv::Point3f& newWorldPosition);

        void updateTrackedPoint(TrackingMatrices& matrices,
                                ScalingCoordinateMapper& scalingMapper,
                                TrackedPoint& trackedPoint);

        cv::Point get_refined_high_res_position(TrackingMatrices& matrices,
                                                const TrackedPoint& trackedPoint);

        void validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                           ScalingCoordinateMapper& scalingMapper,
                                           TrackedPoint& trackedPoint,
                                           const cv::Point& targetPoint);
        bool is_valid_point_area(TrackingMatrices& matrices, const cv::Point& targetPoint);
        void update_tracked_point_from_world_position(TrackedPoint& trackedPoint,
                                                      const cv::Point3f& newWorldPosition,
                                                      const float resizeFactor,
                                                      const CoordinateMapper& fullSizeMapper);

        float m_updateCycleBandwidthDepth;
        float m_createCycleBandwidthDepth;
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
        float m_pointInertiaFactor;
        float m_pointInertiaRadius;
        int m_maxInactiveFramesToBeConsideredActive;
        int m_minActiveFramesToLockTracking;
        int m_maxInactiveFramesForCandidatePoints;
        int m_maxInactiveFramesForLostPoints;
        int m_maxInactiveFramesForActivePoints;
        float m_pointSmoothingFactor;
        float m_pointDeadBandSmoothingFactor;
        float m_pointSmoothingDeadZone;

        int m_nextTrackingId{ 0 };
        //TODO consider std::list<TrackedPoint>
        std::vector<TrackedPoint> m_trackedPoints;

    };

}}}

#endif // POINTPROCESSOR_H