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
                                                      const conversion_cache_t& depthToWorldData);

        PointProcessorSettings& m_settings;
        int m_probationFrameCount { 0 };

        int m_nextTrackingId{ 0 };
        //TODO consider std::list<TrackedPoint>
        std::vector<TrackedPoint> m_trackedPoints;

        std::unordered_map<int, TrajectoryAnalyzer> m_trajectories;
    };

}}}

#endif // POINTPROCESSOR_H
