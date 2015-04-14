#ifndef SEGMENTATIONTRACKER_H
#define SEGMENTATIONTRACKER_H

#include <opencv2/opencv.hpp>
#include "coordinateconverter.h"

struct TrackingData;

class SegmentationUtility
{
public:
    static cv::Point convergeTrackPointFromSeed(TrackingData data);

    static void calculateSegmentArea(cv::Mat& matDepth, cv::Mat& matArea, const CoordinateConverter& converter);
    static void calculateEdgeDistance(cv::Mat& matSegmentation, cv::Mat& matArea, cv::Mat& matEdgeDistance);

    static void calculateBasicScore(cv::Mat& matDepth, cv::Mat& matScore, const float heightFactor, const float depthFactor, const CoordinateConverter& converter);

    static bool findForegroundPixel(cv::Mat& matForeground, cv::Point& foregroundPosition);

    static float countNeighborhoodArea(cv::Mat& matForeground, cv::Mat& matDepth, cv::Mat& matArea, cv::Point_<int> center, const float bandwidth, const float bandwidthDepth, const CoordinateConverter& converter);

private:
    static void segmentForeground(TrackingData data);
    static cv::Point trackPointFromSeed(TrackingData data);

    static float getDepthArea(cv::Point3f& p1, cv::Point3f& p2, cv::Point3f& p3, const CoordinateConverter& converter);

};

#endif // SEGMENTATIONTRACKER_H