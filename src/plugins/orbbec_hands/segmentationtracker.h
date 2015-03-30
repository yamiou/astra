#ifndef SEGMENTATIONTRACKER_H
#define SEGMENTATIONTRACKER_H

#include <opencv2/opencv.hpp>
#include <SenseKitUL.h>

struct TrackingData;

class SegmentationTracker
{
public:
    static cv::Point convergeTrackPointFromSeed(TrackingData data);
    
private:
    static void segmentForeground(TrackingData data);
    static cv::Point trackPointFromSeed(TrackingData data);

};

#endif // SEGMENTATIONTRACKER_H