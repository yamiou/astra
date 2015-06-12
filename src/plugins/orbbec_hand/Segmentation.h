#ifndef SEGMENTATION_H
#define SEGMENTATION_H

#include <opencv2/imgproc/imgproc.hpp>
#include "ScalingCoordinateMapper.h"

namespace sensekit { namespace plugins { namespace hand {

    struct TrackingData;

    namespace segmentation {

        static cv::Point INVALID_POINT(-1, -1);

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

        cv::Point converge_track_point_from_seed(TrackingData& data);

        void visit_circle_circumference(cv::Mat& matDepth,
                                        const cv::Point& center,
                                        const float& radius,
                                        const ScalingCoordinateMapper& mapper,
                                        std::function<void(cv::Point)> callback);

        void visit_circle_circumference_sequential(cv::Mat& matDepth,
                                                   const cv::Point& center,
                                                   const float& radius,
                                                   const ScalingCoordinateMapper& mapper,
                                                   std::function<void(cv::Point)> callback);

        float get_max_sequential_circumference_percentage(cv::Mat& matDepth,
                                                         cv::Mat& matSegmentation,
                                                         const cv::Point& center,
                                                         const float& radius,
                                                         const ScalingCoordinateMapper& mapper);

    }
}}}

#endif // SEGMENTATION_H
