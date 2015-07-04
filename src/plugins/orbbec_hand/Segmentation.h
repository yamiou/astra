#ifndef SEGMENTATION_H
#define SEGMENTATION_H

#include <opencv2/imgproc/imgproc.hpp>
#include "ScalingCoordinateMapper.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackingData;

    enum ForegroundStatus
    {
        FOREGROUND_EMPTY = 0,
        FOREGROUND_HAS_POINTS = 1
    };

    namespace segmentation {

        static cv::Point INVALID_POINT(-1, -1);

        float get_point_area(TrackingMatrices& matrices,
                             AreaTestSettings& settings,
                             const cv::Point& point);

        float get_point_area_integral(TrackingMatrices& matrices,
                                      cv::Mat& integralArea,
                                      AreaTestSettings& settings,
                                      const cv::Point& point);

        bool test_point_in_range(TrackingMatrices& matrices,
                                 const cv::Point& targetPoint,
                                 TestBehavior outputLog);

        bool test_point_area(TrackingMatrices& matrices,
                             AreaTestSettings& settings,
                             const cv::Point& targetPoint,
                             TestPhase phase,
                             TestBehavior outputLog);

        bool test_point_area_integral(TrackingMatrices& matrices,
                                      cv::Mat& integralArea,
                                      AreaTestSettings& settings,
                                      const cv::Point& targetPoint,
                                      TestPhase phase,
                                      TestBehavior outputLog);

        bool test_foreground_radius_percentage(TrackingMatrices& matrices,
                                               CircumferenceTestSettings& settings,
                                               const cv::Point& targetPoint,
                                               TestPhase phase,
                                               TestBehavior outputLog);

        ForegroundStatus create_test_pass_from_foreground(TrackingData& data);

        bool find_next_velocity_seed_pixel(cv::Mat& foregroundMatrix,
                                        cv::Mat& searchedMatrix,
                                        cv::Point& foregroundPosition,
                                        cv::Point& nextSearchStart);

        void calculate_edge_distance(cv::Mat& segmentationMatrix,
                                     cv::Mat& areaSqrtMatrix,
                                     cv::Mat& edgeDistanceMatrix,
                                     const float maxEdgeDistance);

        float count_neighborhood_area(cv::Mat& matSegmentation,
                                      cv::Mat& matDepth,
                                      cv::Mat& matArea,
                                      const cv::Point& center,
                                      const float bandwidth,
                                      const float bandwidthDepth,
                                      const ScalingCoordinateMapper& mapper);

        cv::Mat& calculate_integral_area(TrackingMatrices& matrices);

        float count_neighborhood_area_integral(cv::Mat& matDepth,
                                               cv::Mat& matAreaIntegral,
                                               const cv::Point& center,
                                               const float bandwidth,
                                               const ScalingCoordinateMapper& mapper);

        cv::Point track_point_impl(TrackingData& data);

        cv::Point track_point_from_seed(TrackingData& data);

        void get_circumference_points(cv::Mat& matDepth,
                                      const cv::Point& center,
                                      const float& radius,
                                      const ScalingCoordinateMapper& mapper,
                                      std::vector<sensekit::Vector2i>& points);

        float get_max_sequential_circumference_percentage(cv::Mat& matDepth,
                                                          cv::Mat& matSegmentation,
                                                          const cv::Point& center,
                                                          const float& radius,
                                                          const ScalingCoordinateMapper& mapper,
                                                          std::vector<sensekit::Vector2i>& points);

        float get_percent_natural_edges(cv::Mat& matDepth,
                                        cv::Mat& matSegmentation,
                                        const cv::Point& center,
                                        const float bandwidth,
                                        const ScalingCoordinateMapper& mapper);

        bool test_natural_edges(TrackingMatrices& matrices,
                                NaturalEdgeTestSettings& settings,
                                const cv::Point& targetPoint,
                                TestPhase phase,
                                TestBehavior outputLog);
    }
}}}

#endif // SEGMENTATION_H
