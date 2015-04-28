#ifndef SEGMENTATION_H
#define SEGMENTATION_H

#include "ScalingCoordinateMapper.h"
#include <opencv2/opencv.hpp>

namespace sensekit { namespace plugins { namespace hand {

    struct TrackingData;

    namespace segmentation {

        bool find_next_foreground_pixel(cv::Mat& foregroundMatrix,
                                        cv::Mat& searchedMatrix,
                                        cv::Point& foregroundPosition, 
                                        cv::Point& nextSearchStart);

        void calculate_edge_distance(cv::Mat& segmentationMatrix,
                                     cv::Mat& areaSqrtMatrix,
                                     cv::Mat& edgeDistanceMatrix);

        void calculate_basic_score(cv::Mat& depthMatrix,
                                   cv::Mat& scoreMatrix,
                                   const float heightFactor,
                                   const float depthFactor,
                                   const ScalingCoordinateMapper& mapper);

        void calculate_layer_score(cv::Mat& depthMatrix,
                                   cv::Mat& basicScoreMatrix,
                                   cv::Mat& layerScoreMatrix,
                                   cv::Mat& edgeDistanceMatrix,
                                   const float edgeDistanceFactor,
                                   const float targetEdgeDist);

        void calculate_segment_area(cv::Mat& depthMatrix,
                                    cv::Mat& areaMatrix,
                                    cv::Mat& areaSqrtMatrix,
                                    const ScalingCoordinateMapper& mapper);

        float count_neighborhood_area(cv::Mat& segmentationMatrix,
                                      cv::Mat& depthMatrix,
                                      cv::Mat& areaMatrix,
                                      const cv::Point& center,
                                      const float bandwidth,
                                      const float bandwidthDepth,
                                      const ScalingCoordinateMapper& mapper);

        cv::Point converge_track_point_from_seed(TrackingData data);

    }
}}}

#endif // SEGMENTATION_H