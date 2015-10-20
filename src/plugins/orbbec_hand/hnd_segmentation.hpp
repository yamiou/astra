// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#ifndef HND_SEGMENTATION_H
#define HND_SEGMENTATION_H

#include <opencv2/imgproc/imgproc.hpp>
#include "hnd_scaling_coordinate_mapper.hpp"

namespace astra { namespace hand {

    struct tracking_data;

    enum class foreground_status
    {
        empty = 0,
        has_points = 1
    };

    namespace segmentation {

        static cv::Point INVALID_POINT(-1, -1);

        float get_point_area(tracking_matrices& matrices,
                             area_test_settings& settings,
                             const cv::Point& point);

        float get_point_area_integral(tracking_matrices& matrices,
                                      cv::Mat& integralArea,
                                      area_test_settings& settings,
                                      const cv::Point& point);

        bool test_point_in_range(tracking_matrices& matrices,
                                 const cv::Point& targetPoint,
                                 test_behavior outputLog);

        bool test_point_area(tracking_matrices& matrices,
                             area_test_settings& settings,
                             const cv::Point& targetPoint,
                             test_phase phase,
                             test_behavior outputLog);

        bool test_point_area_integral(tracking_matrices& matrices,
                                      cv::Mat& integralArea,
                                      area_test_settings& settings,
                                      const cv::Point& targetPoint,
                                      test_phase phase,
                                      test_behavior outputLog);

        bool test_foreground_radius_percentage(tracking_matrices& matrices,
                                               circumference_test_settings& settings,
                                               const cv::Point& targetPoint,
                                               test_phase phase,
                                               test_behavior outputLog);

        foreground_status create_test_pass_from_foreground(tracking_data& data);

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
                                      const scaling_coordinate_mapper& mapper);

        cv::Mat& calculate_integral_area(tracking_matrices& matrices);

        float count_neighborhood_area_integral(cv::Mat& matDepth,
                                               cv::Mat& matAreaIntegral,
                                               const cv::Point& center,
                                               const float bandwidth,
                                               const scaling_coordinate_mapper& mapper);

        cv::Point track_point_impl(tracking_data& data);

        cv::Point track_point_from_seed(tracking_data& data);

        void get_circumference_points(cv::Mat& matDepth,
                                      const cv::Point& center,
                                      const float& radius,
                                      const scaling_coordinate_mapper& mapper,
                                      std::vector<astra::vector2i>& points);

        float get_max_sequential_circumference_percentage(cv::Mat& matDepth,
                                                          cv::Mat& matSegmentation,
                                                          const cv::Point& center,
                                                          const float& radius,
                                                          const scaling_coordinate_mapper& mapper,
                                                          std::vector<astra::vector2i>& points);

        float get_percent_natural_edges(cv::Mat& matDepth,
                                        cv::Mat& matSegmentation,
                                        const cv::Point& center,
                                        const float bandwidth,
                                        const scaling_coordinate_mapper& mapper);

        bool test_natural_edges(tracking_matrices& matrices,
                                natural_edge_test_settings& settings,
                                const cv::Point& targetPoint,
                                test_phase phase,
                                test_behavior outputLog);
    }
}}

#endif // HND_SEGMENTATION_H
