#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H

#include <opencv2/core/affine.hpp>
#include "TrackingData.h"
#include "ScalingCoordinateMapper.h"
#include <SenseKit/Plugins/PluginLogger.h>
#include "HandSettings.h"
#include <unordered_map>
#include "TrajectoryAnalyzer.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackedPoint;

    class PointProcessor
    {
    public:
        PointProcessor(PointProcessorSettings& settings);
        virtual ~PointProcessor();

        void initialize_common_calculations(TrackingMatrices& matrices);
        void updateTrackedPoints(TrackingMatrices& matrices);

        void removeDuplicatePoints();
        void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                const cv::Point& seedPosition);
        void removeOldOrDeadPoints();

        void update_full_resolution_points(TrackingMatrices& matrices);

        void update_trajectories();

        std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

        void reset();

        float foregroundRadius1() const { return m_foregroundRadius1; }
        float foregroundRadius2() const { return m_foregroundRadius2; }

        float get_point_area(TrackingMatrices& matrices, const cv::Point& point);
        bool test_point_in_range(TrackingMatrices& matrices,
                                 const cv::Point& targetPoint,
                                 int trackingId,
                                 bool outputLog);
        bool test_point_area(TrackingMatrices& matrices,
                             const cv::Point& targetPoint,
                             int trackingId,
                             bool outputLog);
        bool test_foreground_radius_percentage(TrackingMatrices& matrices,
                                               const cv::Point& targetPoint,
                                               int trackingId,
                                               bool outputLog);
        void calculateTestPassMap(TrackingMatrices& matrices);

    private:
        cv::Point3f smooth_world_positions(const cv::Point3f& oldWorldPosition, const cv::Point3f& newWorldPosition);
        void calculate_area(TrackingMatrices& matrices, ScalingCoordinateMapper mapper);
        void updateTrackedPoint(TrackingMatrices& matrices,
                                ScalingCoordinateMapper& scalingMapper,
                                TrackedPoint& trackedPoint);

        cv::Point3f get_refined_high_res_position(TrackingMatrices& matrices,
                                                  const TrackedPoint& trackedPoint);

        void validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                           ScalingCoordinateMapper& scalingMapper,
                                           TrackedPoint& trackedPoint,
                                           const cv::Point& targetPoint);
        void start_probation(TrackedPoint& trackedPoint);
        void end_probation(TrackedPoint& trackedPoint);
        void update_tracked_point_data(TrackingMatrices& matrices, ScalingCoordinateMapper& scalingMapper, TrackedPoint& trackedPoint, const cv::Point& newTargetPoint);
        void update_tracked_point_from_world_position(TrackedPoint& trackedPoint,
                                                      const cv::Point3f& newWorldPosition,
                                                      const float resizeFactor,
                                                      const CoordinateMapper& fullSizeMapper);

        PointProcessorSettings& m_settings;
        float m_segmentationBandwidthDepthNear;
        float m_segmentationBandwidthDepthFar;
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
        float m_targetEdgeDistance;
        float m_heightScoreFactor;
        float m_depthScoreFactor;
        float m_edgeDistanceScoreFactor;
        float m_pointInertiaFactor;
        float m_pointInertiaRadius;
        int m_maxInactiveFramesForCandidatePoints;
        int m_maxInactiveFramesForLostPoints;
        int m_maxInactiveFramesForActivePoints;
        float m_pointSmoothingFactor;
        float m_pointDeadBandSmoothingFactor;
        float m_pointSmoothingDeadZone;
        float m_foregroundRadius1;
        float m_foregroundRadius2;
        float m_foregroundRadiusMaxPercent1;
        float m_foregroundRadiusMaxPercent2;
        int m_maxFailedTestsInProbation;
        int m_probationFrameCount;
        int m_maxFailedTestsInProbationActivePoints;
        float m_secondChanceMinDistance;

        int m_nextTrackingId{ 0 };
        //TODO consider std::list<TrackedPoint>
        std::vector<TrackedPoint> m_trackedPoints;

        std::unordered_map<int, TrajectoryAnalyzer> m_trajectories;
    };

}}}

#endif // POINTPROCESSOR_H
