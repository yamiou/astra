#ifndef HND_POINT_PROCESSOR_H
#define HND_POINT_PROCESSOR_H

#include <opencv2/core/affine.hpp>
#include "hnd_tracking_data.hpp"
#include "hnd_scaling_coordinate_mapper.hpp"
#include <Astra/Plugins/PluginLogger.h>
#include "hnd_settings.hpp"
#include <unordered_map>
#include "hnd_trajectory_analyzer.hpp"

namespace astra { namespace hand {

    struct tracked_point;

    class point_processor
    {
    public:
        point_processor(point_processor_settings& settings);
        virtual ~point_processor();

        void initialize_common_calculations(tracking_matrices& matrices);
        void update_tracked_points(tracking_matrices& matrices);

        void remove_duplicate_points();
        void update_tracked_or_create_new_point_from_seed(tracking_matrices& matrices,
                                                                 const cv::Point& seedPosition);
        void remove_stale_or_dead_points();

        void update_full_resolution_points(tracking_matrices& matrices);

        void update_trajectories();

        std::vector<tracked_point>& get_trackedPoints() { return trackedPoints_; }

        void reset();

    private:
        cv::Point3f smooth_world_positions(const cv::Point3f& oldWorldPosition, const cv::Point3f& newWorldPosition);
        void calculate_area(tracking_matrices& matrices, scaling_coordinate_mapper mapper);
        void update_tracked_point(tracking_matrices& matrices,
                                 scaling_coordinate_mapper& scalingMapper,
                                 tracked_point& trackedPoint);

        cv::Point3f get_refined_high_res_position(tracking_matrices& matrices,
                                                  const tracked_point& trackedPoint);

        void validate_and_update_tracked_point(tracking_matrices& matrices,
                                            scaling_coordinate_mapper& scalingMapper,
                                            tracked_point& trackedPoint,
                                            const cv::Point& targetPoint);
        void start_probation(tracked_point& trackedPoint);
        void end_probation(tracked_point& trackedPoint);
        void update_tracked_point_data(tracking_matrices& matrices, scaling_coordinate_mapper& scalingMapper, tracked_point& trackedPoint, const cv::Point& newTargetPoint);
        void update_tracked_point_from_world_position(tracked_point& trackedPoint,
                                                      const cv::Point3f& newWorldPosition,
                                                      const float resizeFactor,
                                                      const conversion_cache_t& depthToWorldData);

        point_processor_settings& settings_;

        int nextTrackingId_{ 0 };
        //TODO consider std::list<tracked_point>
        std::vector<tracked_point> trackedPoints_;

        std::unordered_map<int, trajectory_analyzer> trajectories_;
    };

}}

#endif // HND_POINT_PROCESSOR_H
