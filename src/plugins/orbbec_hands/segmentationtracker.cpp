#include "segmentationtracker.h"
#include "trackingdata.h"
#include <queue>


struct PointTTL
{
public:
    cv::Point m_point;
    float m_ttl;
    bool m_pathInRange;

    PointTTL(cv::Point point, float ttl, bool pathInRange)
    {
        m_point = point;
        m_ttl = ttl;
        m_pathInRange = pathInRange;
    }
};

void SegmentationTracker::segmentForeground(TrackingData data)
{
    const float maxTTL = 250; //mm
    float seedDepth = data.matDepth.at<float>(data.seedPosition);
    cv::Mat& matDepth = data.matDepth;
    cv::Mat& matForeground = data.matForegroundSearched;
    cv::Mat& matAreaSqrt = data.matAreaSqrt;
    cv::Mat& matSegmentation = data.matGlobalSegmentation;
    bool isActivePoint = data.pointType == TrackedPointType::ActivePoint;
    std::queue<PointTTL> pointQueue;

    //does the seed point start in range?
    //If not, it will search outward until it finds in range pixels
    const float maxDepth = data.referenceDepth + data.bandwidthDepth;
    bool seedInRange = seedDepth != 0 && seedDepth < maxDepth;
    bool anyInRange = seedInRange;

    pointQueue.push(PointTTL(data.seedPosition, maxTTL, seedInRange));

    cv::Mat matVisited = cv::Mat::zeros(data.matDepth.size(), CV_8UC1);

    int width = data.matDepth.cols;
    int height = data.matDepth.rows;

    matVisited.at<char>(data.seedPosition) = 1;

    while (!pointQueue.empty())
    {
        PointTTL pt = pointQueue.front();
        pointQueue.pop();
        cv::Point p = pt.m_point;
        float ttl = pt.m_ttl;
        bool pathInRange = pt.m_pathInRange;

        if (matForeground.at<char>(p) == PixelType::Foreground)
        {
            ttl = maxTTL;
        }
        if (ttl > 0)
        {
            matForeground.at<char>(p) = PixelType::Searched;

            float depth = matDepth.at<float>(p);
            bool pointInRange = depth != 0 && depth < maxDepth;
            if (ttl > 0 && (!pathInRange || pointInRange))
            {
                //If active tracking, then must be in range to decrement TTL.
                //This will give active points a larger range, more likely to recover.
                //If not active tracking, will always decrement TTL.
                if (!isActivePoint || anyInRange)
                {
                    ttl -= matAreaSqrt.at<float>(p);
                }

                if (pointInRange)
                {
                    //Once a path has "come ashore" -- found an in-range pixel -- it won't leave the range again
                    pathInRange = true;
                    anyInRange = true;
                    matSegmentation.at<char>(p) = PixelType::Foreground;
                }

                cv::Point right(p.x + 1, p.y);
                cv::Point left(p.x - 1, p.y);
                cv::Point down(p.x, p.y + 1);
                cv::Point up(p.x, p.y - 1);

                if (right.x < width && 0 == matVisited.at<char>(right))
                {
                    matVisited.at<char>(right) = 1;
                    pointQueue.push(PointTTL(right, ttl, pathInRange));
                }
                if (left.x >= 0 && 0 == matVisited.at<char>(left))
                {
                    matVisited.at<char>(left) = 1;
                    pointQueue.push(PointTTL(left, ttl, pathInRange));
                }
                if (down.y < height && 0 == matVisited.at<char>(down))
                {
                    matVisited.at<char>(down) = 1;
                    pointQueue.push(PointTTL(down, ttl, pathInRange));
                }
                if (up.y >= 0 && 0 == matVisited.at<char>(up))
                {
                    matVisited.at<char>(up) = 1;
                    pointQueue.push(PointTTL(up, ttl, pathInRange));
                }
            }
        }
    }
}

cv::Point SegmentationTracker::trackPointFromSeed(TrackingData data)
{
    data.matLayerSegmentation = cv::Mat::zeros(data.matGlobalSegmentation.size(), CV_8UC1);

    segmentForeground(data);

    double min, max;
    //for visualization/debugging only

    cv::bitwise_or(data.matLayerSegmentation, data.matGlobalSegmentation, data.matGlobalSegmentation, data.matLayerSegmentation);

    cv::Point minLoc, maxLoc;

    cv::minMaxLoc(data.matScore, &min, &max, &minLoc, &maxLoc, data.matLayerSegmentation);

    return maxLoc;
}

cv::Point SegmentationTracker::convergeTrackPointFromSeed(TrackingData data)
{
    cv::Point point = data.seedPosition;
    cv::Point lastPoint = data.seedPosition;
    int iterations = 0;

    do
    {
        lastPoint = point;
        point = trackPointFromSeed(data);
        ++iterations;
    } while (point != lastPoint && iterations < data.iterationMax && point.x != -1 && point.y != -1);

    return point;
}
