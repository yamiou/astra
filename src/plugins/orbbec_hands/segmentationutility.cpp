#include "segmentationutility.h"
#include "trackingdata.h"
#include <queue>
#include "coordinateconverter.h"

#define MAX_DEPTH 10000

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

void SegmentationUtility::segmentForeground(TrackingData data)
{
    const float maxTTL = 250; //mm
    float seedDepth = data.matrices.matDepth.at<float>(data.seedPosition);
    cv::Mat& matDepth = data.matrices.matDepth;
    cv::Mat& matForeground = data.matrices.matForeground;
    cv::Mat& matArea = data.matrices.matArea;
    cv::Mat& matSegmentation = data.matrices.matLayerSegmentation;
    bool isActivePoint = data.pointType == TrackedPointType::ActivePoint;
    std::queue<PointTTL> pointQueue;

    //does the seed point start in range?
    //If not, it will search outward until it finds in range pixels
    const float maxDepth = data.referenceDepth + data.bandwidthDepth;
    bool seedInRange = seedDepth != 0 && seedDepth < maxDepth;
    bool anyInRange = seedInRange;

    pointQueue.push(PointTTL(data.seedPosition, maxTTL, seedInRange));

    cv::Mat matVisited = cv::Mat::zeros(matDepth.size(), CV_8UC1);

    int width = matDepth.cols;
    int height = matDepth.rows;

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
                    ttl -= sqrt(matArea.at<float>(p));
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

cv::Point SegmentationUtility::trackPointFromSeed(TrackingData data)
{
    data.matrices.matLayerSegmentation = cv::Mat::zeros(data.matrices.matDepth.size(), CV_8UC1);

    segmentForeground(data);

    double min, max;
    cv::Point minLoc, maxLoc;

    cv::minMaxLoc(data.matrices.matScore, &min, &max, &minLoc, &maxLoc, data.matrices.matLayerSegmentation);

    return maxLoc;
}

cv::Point SegmentationUtility::convergeTrackPointFromSeed(TrackingData data)
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


bool SegmentationUtility::findForegroundPixel(cv::Mat& matForeground, cv::Point& foregroundPosition)
{
    int width = matForeground.cols;
    int height = matForeground.rows;

    for (int y = 0; y < height; y++)
    {
        char* foregroundRow = matForeground.ptr<char>(y);
        for (int x = 0; x < width; x++)
        {
            char foreground = *foregroundRow;

            if (foreground == PixelType::Foreground)
            {
                foregroundPosition.x = x;
                foregroundPosition.y = y;
                *foregroundRow = PixelType::Searched;
                return true;
            }
            ++foregroundRow;
        }
    }
    foregroundPosition.x = -1;
    foregroundPosition.y = -1;
    return false;
}

void SegmentationUtility::calculateBasicScore(cv::Mat& matDepth, cv::Mat& matScore, const float heightFactor, const float depthFactor, const CoordinateConverter& converter)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    for (int y = 0; y < height; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        float* scoreRow = matScore.ptr<float>(y);

        for (int x = 0; x < width; x++)
        {
            float depth = *depthRow;
            if (depth != 0)
            {
                cv::Point3f worldPosition = converter.convertDepthToRealWorld(x, y, depth);

                float score = worldPosition.y * heightFactor;
                score += (MAX_DEPTH - worldPosition.z) * depthFactor;

                *scoreRow = score;
            }
            else
            {
                *scoreRow = 0;
            }
            ++depthRow;
            ++scoreRow;
        }
    }
}

void SegmentationUtility::calculateEdgeDistance(cv::Mat& matSegmentation, cv::Mat& matArea, cv::Mat& matEdgeDistance)
{
    cv::Mat eroded, temp;
    cv::Mat crossElement = cv::getStructuringElement(cv::MORPH_CROSS, cv::Size(3, 3));


    matEdgeDistance = cv::Mat::zeros(matSegmentation.size(), CV_32FC1);
    cv::Mat ones = cv::Mat::ones(matSegmentation.size(), CV_32FC1);
    matSegmentation.copyTo(eroded);

    //close small holes
    int dilateCount = 1;
    for (int i = 0; i < dilateCount; i++)
    {
        cv::dilate(eroded, eroded, crossElement);
    }

    int nonZeroCount = 0;
    const int imageLength = eroded.cols * eroded.rows;
    int iterations = 0;
    const int maxIterations = matSegmentation.cols / 2;
    bool done;
    do
    {
        //erode makes the image smaller
        cv::erode(eroded, eroded, crossElement);
        //accumulate the eroded image to the matGlobalSegmentation buffer
        cv::add(matArea, matEdgeDistance, matEdgeDistance, eroded, CV_32FC1);

        nonZeroCount = cv::countNonZero(eroded);
        done = (nonZeroCount == 0);

        //nonZerCount < imageLength guards against image with all 1's, which will never erode
    } while (!done && nonZeroCount < imageLength && ++iterations < maxIterations);
}

void SegmentationUtility::calculateSegmentArea(cv::Mat& matDepth, cv::Mat& matArea, const CoordinateConverter& converter)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    matArea = cv::Mat::zeros(matDepth.size(), CV_32FC1);

    for (int y = 0; y < height - 1; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        float* nextDepthRow = matDepth.ptr<float>(y + 1);
        float* areaRow = matArea.ptr<float>(y);

        for (int x = 0; x < width - 1; x++)
        {
            float area = 0;
            float depth1 = *depthRow;

            if (depth1 != 0)
            {
                cv::Point3f p1(x, y, depth1);
                cv::Point3f p2(x + 1, y, depth1);
                cv::Point3f p3(x, y + 1, depth1);
                cv::Point3f p4(x + 1, y + 1, depth1);

                area += getDepthArea(p1, p2, p3, converter);
                area += getDepthArea(p2, p3, p4, converter);
            }

            *areaRow = area;

            ++depthRow;
            ++nextDepthRow;
            ++areaRow;
        }
    }
}

float SegmentationUtility::getDepthArea(cv::Point3f& p1, cv::Point3f& p2, cv::Point3f& p3, const CoordinateConverter& converter)
{
    cv::Point3f world1 = converter.convertDepthToRealWorld(p1);
    cv::Point3f world2 = converter.convertDepthToRealWorld(p2);
    cv::Point3f world3 = converter.convertDepthToRealWorld(p3);

    cv::Point3f v1 = world2 - world1;
    cv::Point3f v2 = world3 - world1;

    float area = static_cast<float>(0.5 * cv::norm(v1.cross(v2)));
    return area;
}

float SegmentationUtility::countNeighborhoodArea(cv::Mat& matSegmentation, cv::Mat& matDepth, cv::Mat& matArea, cv::Point center, const float bandwidth, const float bandwidthDepth, const CoordinateConverter& converter)
{
    float startingDepth = matDepth.at<float>(center);

    cv::Point topLeft = converter.offsetPixelLocationByMM(center, -bandwidth, bandwidth, startingDepth);
    cv::Point bottomRight = converter.offsetPixelLocationByMM(center, bandwidth, -bandwidth, startingDepth);
    int32_t x0 = MAX(0, topLeft.x);
    int32_t y0 = MAX(0, topLeft.y);
    int32_t x1 = MIN(matDepth.cols - 1, bottomRight.x);
    int32_t y1 = MIN(matDepth.rows - 1, bottomRight.y);

    float area = 0;

    for (int y = y0; y < y1; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        char* segmentationRow = matSegmentation.ptr<char>(y);
        float* areaRow = matArea.ptr<float>(y);

        depthRow += x0;
        segmentationRow += x0;
        areaRow += x0;
        for (int x = x0; x < x1; x++)
        {
            if (*segmentationRow == PixelType::Foreground)
            {
                float depth = *depthRow;
                if (abs(depth - startingDepth) < bandwidthDepth)
                {
                    area += *areaRow;
                }
            }
            ++depthRow;
            ++areaRow;
            ++segmentationRow;
        }
    }

    return area;
}
