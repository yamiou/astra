// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif


#include "handtracker.h"
#include <math.h>

#include <cstdint>
#include <queue>
#include <ctime>
#include "frameconverter.h"

using namespace std;

#define GL_WIN_SIZE_X   640
#define GL_WIN_SIZE_Y   480

#define PROCESSING_SIZE_X 80
#define PROCESSING_SIZE_Y 60

#define TEXTURE_SIZE    512

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))

HandTracker::HandTracker(sensekit_depthstream_t* depthStream) :
m_depthStream(depthStream)
{
    mouseX = 0;
    mouseY = 0;
    setupVariables();
}

HandTracker::~HandTracker()
{
}

void HandTracker::onKey(unsigned char key)
{
    float delta = 0.0001;
    float delta2 = 0.01;
    float delta3 = 0.001;
    switch (key)
    {
    case '`':
        m_showForeground = !m_showForeground;
        break;
    case 'c':
        m_showCircles = !m_showCircles;
        break;
    case '~':
        m_factor1 = 1.0;
        m_factor2 = 1.0;
        m_factor3 = 1.0;
        m_factor4 = 1.0;
        break;
    case '!':
        m_factor1 = 1.0;
        m_factor2 = 0.0;
        m_factor3 = 0.0;
        m_factor4 = 0.0;
        break;
    case '@':
        m_factor1 = 0.0;
        m_factor2 = 1.0;
        m_factor3 = 0.0;
        m_factor4 = 0.0;
        break;
    case '#':
        m_factor1 = 0.0;
        m_factor2 = 0.0;
        m_factor3 = 1.0;
        m_factor4 = 0.0;
        break;
    case '$':
        m_factor1 = 0.0;
        m_factor2 = 0.0;
        m_factor3 = 0.0;
        m_factor4 = 1.0;
        break;
    case 'j':
        m_maxVelocity -= delta3;
        if (m_maxVelocity < delta3)
        {
            m_maxVelocity = delta3;
        }
        printf("max velocity: %f\n", m_maxVelocity);
        break;
    case 'l':
        m_maxVelocity += delta3;
        printf("max velocity: %f\n", m_maxVelocity);
        break;
    case 'v':
        m_outputSample = !m_outputSample;
        break;
    case 'r':
        m_foregroundThresholdFactor += delta;
        printf("foreground threshold velocity: %f\n", m_foregroundThresholdFactor);
        break;
    case 'e':
        m_foregroundThresholdFactor -= delta;
        printf("foreground threshold velocity: %f\n", m_foregroundThresholdFactor);
        break;
    case 'w':
        m_maxVelocityFactor += delta;
        printf("max velocity: %f\n", m_maxVelocityFactor);
        break;
    case 'q':
        m_maxVelocityFactor -= delta;
        printf("max velocity: %f\n", m_maxVelocityFactor);
        break;
    case 's':
        m_depthSmoothingFactor += delta2;
        printf("Depth smoothing: %f\n", m_depthSmoothingFactor);
        break;
    case 'a':
        m_depthSmoothingFactor -= delta2;
        printf("Depth smoothing: %f\n", m_depthSmoothingFactor);
        break;
    case 'x':
        m_velocitySmoothingFactor += delta2;
        printf("Velocity smoothing: %f\n", m_velocitySmoothingFactor);
        break;
    case 'z':
        m_velocitySmoothingFactor -= delta2;
        printf("Velocity smoothing: %f\n", m_velocitySmoothingFactor);
        break;
    }

}

void HandTracker::filterZeroValuesAndJumps(cv::Mat matDepth, cv::Mat matDepthPrevious, cv::Mat matDepthAvg, cv::Mat matDepthVel, float maxDepthJumpPercent)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    for (int y = 0; y < height; ++y)
    {
        float* resizedRow = matDepth.ptr<float>(y);
        float* prevRow = matDepthPrevious.ptr<float>(y);
        float* avgRow = matDepthAvg.ptr<float>(y);
        float* velRow = matDepthVel.ptr<float>(y);

        for (int x = 0; x < width; ++x)
        {
            float depth = *resizedRow;
            float previousDepth = *prevRow;

            //calculate percent change since last frame
            float deltaPercent = abs(depth - previousDepth) / previousDepth;
            //if this frame or last frame are zero depth, or there is a large jump
            if (0 == depth || 0 == previousDepth || (deltaPercent > maxDepthJumpPercent && deltaPercent > 0))
            {
                //set the average to the current depth, and set velocity to zero
                //this suppresses the velocity signal for edge jumping artifacts
                avgRow[x] = depth;
                velRow[x] = 0;
            }

            *prevRow = depth;

            ++resizedRow;
            ++prevRow;
        }
    }
}

void HandTracker::thresholdForeground(cv::Mat& matForeground, cv::Mat& matVelocity, float foregroundThresholdFactor)
{
    int width = matForeground.cols;
    int height = matForeground.rows;

    for (int y = 0; y < height; ++y)
    {
        float* velRow = matVelocity.ptr<float>(y);
        char* foregroundRow = matForeground.ptr<char>(y);

        for (int x = 0; x < width; ++x)
        {
            float vel = *velRow;
            if (vel > foregroundThresholdFactor)
            {
                *foregroundRow = PixelType::Foreground;
            }
            else
            {
                *foregroundRow = PixelType::Background;
            }

            ++foregroundRow;
            ++velRow;
        }
    }
}

cv::Point3f HandTracker::convertDepthToRealWorld(float localX, float localY, float localZ)
{
    float worldX, worldY, worldZ;

    localX *= m_resizeFactor;
    localY *= m_resizeFactor;

    convert_depth_to_world(localX, localY, localZ, &worldX, &worldY, &worldZ);

    return cv::Point3f(worldX, worldY, worldZ);
}

vector<TrackedPoint>& HandTracker::updateOriginalPoints(vector<TrackedPoint>& internalTrackedPoints)
{
    m_originalTrackedPoints.clear();

    for (auto it = internalTrackedPoints.begin(); it != internalTrackedPoints.end(); ++it)
    {
        TrackedPoint internalPoint = *it;

        TrackedPoint originalPoint = internalPoint;
        originalPoint.m_position.x *= m_resizeFactor;
        originalPoint.m_position.y *= m_resizeFactor;

        m_originalTrackedPoints.push_back(originalPoint);
    }

    return m_originalTrackedPoints;
}

cv::Point3f HandTracker::convertDepthToRealWorld(cv::Point3f localPosition)
{
    return convertDepthToRealWorld(localPosition.x, localPosition.y, localPosition.z);
}

cv::Point3f HandTracker::convertRealWorldToDepth(cv::Point3f worldPosition)
{
    float localX, localY, localZ;

    convert_world_to_depth(worldPosition.x, worldPosition.y, worldPosition.z, &localX, &localY, &localZ);

    localX /= m_resizeFactor;
    localY /= m_resizeFactor;

    return cv::Point3f(localX, localY, localZ);
}

bool HandTracker::findForegroundPixel(cv::Mat& matForeground, cv::Point& foregroundPosition)
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

cv::Point HandTracker::shiftNearest(cv::Mat& matForeground, cv::Mat& matDepth, const float bandwidth, const float bandwidthDepth, cv::Point start, float& distance)
{
    float startingDepth = matDepth.at<float>(start.y, start.x);

    cv::Point topLeft = offsetPixelLocationByMM(start, -bandwidth, bandwidth, startingDepth);
    cv::Point bottomRight = offsetPixelLocationByMM(start, bandwidth, -bandwidth, startingDepth);
    int32_t x0 = MAX(0, topLeft.x);
    int32_t y0 = MAX(0, topLeft.y);
    int32_t x1 = MIN(matDepth.cols - 1, bottomRight.x);
    int32_t y1 = MIN(matDepth.rows - 1, bottomRight.y);

    float bandwidth2 = bandwidth * bandwidth;

    if (0 == startingDepth)
    {
        startingDepth = MAX_DEPTH;
    }

    cv::Point closestPoint = start;
    float closestDepth = startingDepth;

    for (int y = y0; y < y1; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        char* foregroundRow = matForeground.ptr<char>(y);
        depthRow += x0;
        foregroundRow += x0;
        for (int x = x0; x < x1; x++)
        {
            float depth = *depthRow;

            cv::Point v(x, y);
            auto delta = (start - v);
            float deltaDepth = abs(depth - startingDepth);
            if (delta.x * delta.x + delta.y * delta.y <= bandwidth2 &&
                deltaDepth < bandwidthDepth)
            {
                if (*foregroundRow == PixelType::Foreground)
                {
                    *foregroundRow = PixelType::Searched;
                }
                if (depth != 0 && depth < closestDepth)
                {
                    closestPoint.x = x;
                    closestPoint.y = y;
                    closestDepth = depth;
                }
            }
            ++foregroundRow;
            ++depthRow;
        }
    }

    //*matForeground.ptr<float>(closestPoint.y, closestPoint.x) = PixelType::IntermediateClosest;
    auto deltaMoved = (closestPoint - start);
    distance = sqrt(deltaMoved.x * deltaMoved.x + deltaMoved.y * deltaMoved.y);

    return closestPoint;
}

cv::Point HandTracker::findClosestPixelFromSeed(cv::Mat& matForeground, cv::Mat& matDepth, cv::Point foregroundPosition)
{
    cv::Point closestPoint = foregroundPosition;

    int width = matForeground.cols;
    int height = matForeground.rows;

    const float bandwidth = 200; //mm radius
    const float bandwidthDepth = 200; //mm
    const uint32_t maxIterations = 10;
    const float maxTolerance = 0.5; //pixels

    uint32_t iteration = 0;
    float distance = FLT_MAX;

    while (++iteration <= maxIterations
        && distance > maxTolerance)
    {
        cv::Point previousClosestPoint = closestPoint;
        closestPoint = shiftNearest(matForeground, matDepth, bandwidth, bandwidthDepth, closestPoint, distance);

        if (previousClosestPoint == closestPoint)
            break;
    }

    return closestPoint;
}

void HandTracker::segmentForeground(TrackingData data)
{
    const float maxTTL = 250; //mm
    float seedDepth = data.matDepth.at<float>(data.seedPosition);
    cv::Mat& matDepth = data.matDepth;
    cv::Mat& matForeground = data.matForegroundSearched;
    cv::Mat& matAreaSqrt = data.matAreaSqrt;
    cv::Mat& matSegmentation = data.matGlobalSegmentation;
    bool isActivePoint = data.pointType == TrackedPointType::ActivePoint;
    queue<PointTTL> pointQueue;

    //does the seed point start in range?
    //If not, it will search outward until it finds in range pixels
    const float maxDepth = data.referenceDepth + data.bandwidthDepth;
    bool seedInRange = seedDepth != 0 && seedDepth < maxDepth;
    bool anyInRange = seedInRange;

    pointQueue.push(PointTTL(data.seedPosition, maxTTL, seedInRange));

    cv::Mat matVisited = cv::Mat::zeros(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_8UC1);

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

void HandTracker::calculateBasicScore(cv::Mat& matDepth, cv::Mat& matScore)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    float heightFactor = 1 * m_factor1;
    float depthFactor = 1.5 * m_factor2;

    for (int y = 0; y < height; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        float* scoreRow = matScore.ptr<float>(y);

        for (int x = 0; x < width; x++)
        {
            float depth = *depthRow;
            if (depth != 0)
            {
                cv::Point3f worldPosition = convertDepthToRealWorld(x, y, depth);

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

void HandTracker::calculateEdgeDistance(cv::Mat& matSegmentation, cv::Mat& matArea, cv::Mat& matEdgeDistance)
{
    cv::Mat eroded, temp;

    matEdgeDistance = cv::Mat::zeros(matSegmentation.size(), CV_32FC1);
    cv::Mat ones = cv::Mat::ones(matSegmentation.size(), CV_32FC1);
    matSegmentation.copyTo(eroded);

    //close small holes
    int dilateCount = 1;
    for (int i = 0; i < dilateCount; i++)
    {
        cv::dilate(eroded, eroded, m_crossElement);
    }

    int nonZeroCount = 0;
    const int imageLength = eroded.cols * eroded.rows;
    int iterations = 0;
    const int maxIterations = PROCESSING_SIZE_X / 2;
    bool done;
    do
    {
        //erode makes the image smaller
        cv::erode(eroded, eroded, m_crossElement);
        //accumulate the eroded image to the matGlobalSegmentation buffer
        cv::add(matArea, matEdgeDistance, matEdgeDistance, eroded, CV_32FC1);

        nonZeroCount = cv::countNonZero(eroded);
        done = (nonZeroCount == 0);

        //nonZerCount < imageLength guards against image with all 1's, which will never erode
    } while (!done && nonZeroCount < imageLength && ++iterations < maxIterations);

    /*
        for (int i = 0; i < 10; i++)
        {
        cv::dilate(matEdgeDistance, matEdgeDistance, element);
        }
        */
    /*

    cv::Mat skel;
    cv::dilate(matGlobalSegmentation, skel, element);
    thinning(skel, skel);

    cv::Mat skelViz = skel * 255;
    cv::namedWindow("skel", 0);
    cv::resizeWindow("skel", 640, 480);
    cv::imshow("skel", skelViz);

    int width = matEdgeDistance.cols;
    int height = matEdgeDistance.rows;

    for (int i = 0; i < iterations; i++)
    {
    for (int y = 1; y < height - 1; y++)
    {
    float* lastEdgeRow = matEdgeDistance.ptr<float>(y - 1);
    float* edgeRow = matEdgeDistance.ptr<float>(y);
    float* nextEdgeRow = matEdgeDistance.ptr<float>(y + 1);
    char* lastSkelRow = skel.ptr<char>(y - 1);
    char* skelRow = skel.ptr<char>(y);
    char* nextSkelRow = skel.ptr<char>(y + 1);
    for (int x = 1; x < width - 1; x++)
    {
    if (0 == *skelRow && 0 != *edgeRow)
    {
    float right = FLT_MIN;
    if (0 != *(skelRow+1)) right = *(edgeRow + 1);
    float left = FLT_MIN;
    if (0 != *(skelRow - 1)) left = *(edgeRow - 1);
    float up = FLT_MIN;
    if (0 != *lastSkelRow) up = *lastEdgeRow;
    float down = FLT_MIN;
    if (0 != *(nextSkelRow)) down = *nextEdgeRow;
    float maxval = MAX(MAX(right, left), MAX(up, down));
    if (maxval > 0)
    {
    *edgeRow = maxval;
    *skelRow = 1;
    }
    else
    {
    *edgeRow = 0;
    }
    }
    ++lastSkelRow;
    ++skelRow;
    ++nextSkelRow;
    ++lastEdgeRow;
    ++edgeRow;
    ++nextEdgeRow;
    }
    }
    }*/
}

void HandTracker::calculateSegmentArea(cv::Mat& matDepth, cv::Mat& matArea, cv::Mat& matAreaSqrt)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    matArea = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    matAreaSqrt = cv::Mat::zeros(matDepth.size(), CV_32FC1);

    for (int y = 0; y < height - 1; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        float* nextDepthRow = matDepth.ptr<float>(y + 1);
        float* areaRow = matArea.ptr<float>(y);
        float* areaSqrtRow = matAreaSqrt.ptr<float>(y);

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

                area += getDepthArea(p1, p2, p3);
                area += getDepthArea(p2, p3, p4);
            }

            *areaRow = area;
            *areaSqrtRow = sqrt(area);

            ++depthRow;
            ++nextDepthRow;
            ++areaRow;
            ++areaSqrtRow;
        }
    }
}

float HandTracker::getDepthArea(cv::Point3f& p1, cv::Point3f& p2, cv::Point3f& p3)
{
    float worldX1, worldY1, worldZ1;
    float worldX2, worldY2, worldZ2;
    float worldX3, worldY3, worldZ3;

    cv::Point3f world1 = convertDepthToRealWorld(p1);
    cv::Point3f world2 = convertDepthToRealWorld(p2);
    cv::Point3f world3 = convertDepthToRealWorld(p3);

    cv::Point3f v1 = world2 - world1;
    cv::Point3f v2 = world3 - world1;

    float area = 0.5 * cv::norm(v1.cross(v2));
    return area;
}

cv::Point HandTracker::offsetPixelLocationByMM(cv::Point& position, float offsetX, float offsetY, float depth)
{
    cv::Point3f world = convertDepthToRealWorld(position.x, position.y, depth);

    world.x += offsetX;
    world.y += offsetY;

    cv::Point3f offsetLocal = convertRealWorldToDepth(world);

    return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
}

float HandTracker::countNeighborhoodArea(cv::Mat& matSegmentation, cv::Mat& matDepth, cv::Mat& matArea, const float bandwidth, const float bandwidthDepth, cv::Point center)
{
    float startingDepth = matDepth.at<float>(center);

    cv::Point topLeft = offsetPixelLocationByMM(center, -bandwidth, bandwidth, startingDepth);
    cv::Point bottomRight = offsetPixelLocationByMM(center, bandwidth, -bandwidth, startingDepth);
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

void HandTracker::calculateLocalArea(cv::Mat& matSegmentation, cv::Mat& matDepth, cv::Mat& matArea, cv::Mat& matLocalArea, const float areaBandwidth, const float areaBandwidthDepth)
{
    int width = matDepth.cols;
    int height = matDepth.rows;

    for (int y = 0; y < height; y++)
    {
        float* depthRow = matDepth.ptr<float>(y);
        char* segmentationRow = matSegmentation.ptr<char>(y);
        float* localAreaRow = matLocalArea.ptr<float>(y);

        for (int x = 0; x < width; x++)
        {
            float depth = *depthRow;
            char segmentation = *segmentationRow;

            if (depth != 0 && segmentation == PixelType::Foreground)
            {
                float localArea = countNeighborhoodArea(matSegmentation, matDepth, matArea, areaBandwidth, areaBandwidthDepth, cv::Point(x, y));
                *localAreaRow = localArea;
            }
            ++depthRow;
            ++segmentationRow;
            ++localAreaRow;
        }
    }
}

float HandTracker::calculatePercentForeground(cv::Mat& matSegmentation, cv::Point2f center, int radius)
{
    int width = matSegmentation.cols;
    int height = matSegmentation.rows;

    int cx = center.x;
    int cy = center.y;
    int x0 = MAX(0, cx - radius);
    int y0 = MAX(0, cy - radius);
    int x1 = MIN(matSegmentation.cols - 1, cx + radius);
    int y1 = MIN(matSegmentation.rows - 1, cy + radius);

    int totalCount = 0;
    int foregroundCount = 0;
    int radius2 = radius*radius;
    for (int y = y0; y < y1; y++)
    {
        char* segmentationRow = matSegmentation.ptr<char>(y);

        segmentationRow += x0;
        for (int x = x0; x < x1; x++)
        {
            int dx = x - cx;
            int dy = y - cy;
            if (dx*dx + dy*dy < radius2)
            {
                if (*segmentationRow == PixelType::Foreground)
                {
                    ++foregroundCount;
                }
                ++totalCount;
            }
            ++segmentationRow;
        }
    }

    return foregroundCount / static_cast<float>(totalCount);
}

bool HandTracker::findUnvisitedForegroundPoint(cv::Mat& matSegmentation, cv::Mat& matVisited, cv::Point& position)
{
    int width = matSegmentation.cols;
    int height = matSegmentation.rows;

    for (int y = 0; y < height; y++)
    {
        char* segmentationRow = matSegmentation.ptr<char>(y);
        char* visitedRow = matVisited.ptr<char>(y);
        for (int x = 0; x < width; x++)
        {
            if (*segmentationRow == PixelType::Foreground && *visitedRow == 0)
            {
                position = cv::Point(x, y);
                return true;
            }

            ++segmentationRow;
            ++visitedRow;
        }
    }
    return false;
}

void HandTracker::processCircleAnalysis(cv::Mat& matSegmentation)
{
    cv::Mat matVisited = cv::Mat::zeros(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_8UC1);

    int width = matSegmentation.cols;
    int height = matSegmentation.rows;

    cv::Mat gray, img, src;
    gray.create(matSegmentation.size(), CV_8UC1);
    src.create(matSegmentation.size(), CV_8UC3);
    cv::inRange(matSegmentation, PixelType::Foreground, PixelType::Foreground, gray);
    cv::cvtColor(gray, src, CV_GRAY2RGB);

    cv::Point seed;
    while (findUnvisitedForegroundPoint(matSegmentation, matVisited, seed))
    {
        queue<cv::Point> pointQueue;
        vector<cv::Point> pointList;

        pointQueue.push(seed);

        int count = 0;
        int countMax = 10;
        float minFillPercent = 0.95;
        int minListSize = 30;

        bool shouldContinue = true;
        while (shouldContinue && !pointQueue.empty())
        {
            cv::Point p = pointQueue.front();
            pointQueue.pop();

            char segmentation = matSegmentation.at<char>(p.y, p.x);

            if (segmentation == PixelType::Foreground && matVisited.at<char>(p) == 0)
            {
                src.at<cv::Point3_<uchar>>(p) = cv::Point3_<uchar>(0, 0, 255);
                matVisited.at<char>(p) = 1;
                cv::Point2f center;
                float radius = 0;

                pointList.push_back(p);

                cv::minEnclosingCircle(pointList, center, radius);
                int radiusI = cvRound(radius);
                src.copyTo(img);
                circle(img, center, radiusI, cv::Scalar(255, 0, 0), 1, 8);

                float percentForeground = calculatePercentForeground(matSegmentation, center, radiusI);

                if (pointList.size() > minListSize && percentForeground < minFillPercent)
                {
                    shouldContinue = false;
                }

                cv::Point right(p.x + 1, p.y);
                cv::Point left(p.x - 1, p.y);
                cv::Point down(p.x, p.y + 1);
                cv::Point up(p.x, p.y - 1);

                if (right.x < width && 0 == matVisited.at<char>(right))
                {
                    //m_matVisited.at<char>(right) = 1;
                    pointQueue.push(right);
                }
                if (left.x >= 0 && 0 == matVisited.at<char>(left))
                {
                    //m_matVisited.at<char>(left) = 1;
                    pointQueue.push(left);
                }
                if (down.y < height && 0 == matVisited.at<char>(down))
                {
                    //m_matVisited.at<char>(down) = 1;
                    pointQueue.push(down);
                }
                if (up.y >= 0 && 0 == matVisited.at<char>(up))
                {
                    //m_matVisited.at<char>(up) = 1;
                    pointQueue.push(up);
                }
            }
        }

        img.copyTo(src);
    }

    if (m_showCircles)
    {
        cv::namedWindow("circles", 0);
        cv::resizeWindow("circles", 640, 480);

        cv::imshow("circles", src);
    }
}

cv::Point HandTracker::trackPointFromSeed(TrackingData data)
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

cv::Point HandTracker::convergeTrackPointFromSeed(TrackingData data)
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

void HandTracker::validateAndUpdateTrackedPoint(cv::Mat& matDepth, cv::Mat& matArea, TrackedPoint& tracked, cv::Point targetPoint)
{
    bool updatedPoint = false;
    const float steadyDist = 150; //mm

    const float maxJumpDist = 450; //mm

    const float areaAdaptationFactor = 0.1;
    const float velocityAdaptationFactor = 0.5;
    const float maxWrongAreaCount = 60;

    if (targetPoint.x != -1 && targetPoint.y != -1)
    {
        float depth = matDepth.at<float>(targetPoint);

        cv::Point3f worldPosition = convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth);

        auto dist = cv::norm(worldPosition - tracked.m_worldPosition);
        auto deadbandDist = cv::norm(worldPosition - tracked.m_steadyWorldPosition);

        float area = countNeighborhoodArea(m_tempLayerSegmentation, matDepth, matArea, m_areaBandwidth, m_areaBandwidthDepth, targetPoint);

        if (dist < maxJumpDist && area > m_minArea && area < m_maxArea)
        {
            updatedPoint = true;
            cv::Point3f worldVelocity = worldPosition - tracked.m_worldPosition;
            tracked.m_worldPosition = worldPosition;
            tracked.m_worldVelocity = worldVelocity;

            tracked.m_position = targetPoint;
            if (deadbandDist > steadyDist)
            {
                tracked.m_steadyWorldPosition = worldPosition;
                tracked.m_inactiveFrameCount = 0;
            }

            if (tracked.m_inactiveFrameCount < 10)
            {
                tracked.m_activeFrameCount++;
                if (tracked.m_activeFrameCount > 120)
                {
                    tracked.m_type = TrackedPointType::ActivePoint;
                }
            }

            /*tracked.m_avgArea = (1 - areaAdaptationFactor) * tracked.m_avgArea + areaAdaptationFactor * area;

            if (tracked.m_avgArea > m_minArea && tracked.m_avgArea < m_maxArea)
            {
            tracked.m_wrongAreaCount = 0;
            }
            else
            {
            tracked.m_wrongAreaCount++;
            tracked.m_status = TrackingStatus::Lost;
            if (tracked.m_wrongAreaCount > maxWrongAreaCount)
            {
            tracked.m_status = TrackingStatus::Dead;
            }
            }*/
        }
    }

    if (tracked.m_status != TrackingStatus::Dead)
    {
        if (updatedPoint)
        {
            tracked.m_status = TrackingStatus::Tracking;
        }
        else
        {
            tracked.m_status = TrackingStatus::Lost;
        }
    }
}

void HandTracker::trackPoints(cv::Mat& matForeground, cv::Mat& matDepth, cv::Mat& matSegmentation, cv::Mat& matScore, cv::Mat& edgeDistance, cv::Mat& matArea)
{
    //TODO-done try tracking without edge distance
    //TODO-done calculate global score once
    //TODO-done adjust scores so hand can go below elbow
    //TODO-done use velocity to predict next position - search there as well
    //TODO-done adopt the min tracking id? or id of the most active parent?
    //TODO-done recover tracking IDs for recently lost points (maybe after focus gesture)
    //TODO-done look at head area being allowed
    //TODO-done make a lost active tracking state with lower count for removal
    //TODO-done make new points look for nearby lost active tracking points
    //TODO-done reject tracking updates that move the point to a too large area (prevent hand point from jumping to head and not recovering)
    //TODO calculate refined tracking position (with high res image and edge distance) for tracked points, not intermediate
    //TODO optimization - memoize best scoring position during segmentation step
    //TODO ?make dead points go to lost tracking instead so they can recover (only use dead for duplicate...rename status?)
    //TODO look at initial points jumping to nearby desk instead of hand, then never leaving

    cv::Mat foregroundSearched = matForeground.clone();
    cv::Point seedPosition;

    matSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
    matScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    edgeDistance = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    matArea = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    cv::Mat areaSqrt = cv::Mat::zeros(matDepth.size(), CV_32FC1);

    const float trackingBandwidthDepth = 150;
    const float initialBandwidthDepth = 450;
    const float width = matDepth.cols;
    const float height = matDepth.rows;
    const float maxMatchDistLostActive = 500; //mm
    const float maxMatchDistDefault = 200; //mm
    const int iterationMaxTracking = 1;
    const int iterationMaxInitial = 1;

    calculateBasicScore(matDepth, matScore);
    calculateSegmentArea(matDepth, matArea, areaSqrt);

    //update existing points
    for (auto iter = m_internalTrackedPoints.begin(); iter != m_internalTrackedPoints.end(); ++iter)
    {
        TrackedPoint& tracked = *iter;
        tracked.m_inactiveFrameCount++;
        seedPosition = tracked.m_position;
        float referenceDepth = tracked.m_worldPosition.z;

        TrackingData updateTrackingData(referenceDepth, trackingBandwidthDepth, tracked.m_type, iterationMaxTracking);

        updateTrackingData.matDepth = matDepth;
        updateTrackingData.matAreaSqrt = areaSqrt;
        updateTrackingData.matGlobalSegmentation = matSegmentation;
        updateTrackingData.matScore = matScore;
        updateTrackingData.matForegroundSearched = foregroundSearched;
        updateTrackingData.matLayerSegmentation = m_tempLayerSegmentation;
        updateTrackingData.seedPosition = seedPosition;

        cv::Point targetPoint = convergeTrackPointFromSeed(updateTrackingData);

        validateAndUpdateTrackedPoint(matDepth, matArea, tracked, targetPoint);

        //lost a tracked point, try to guest the position using previous velocity for second chance to recover
        if (tracked.m_status != TrackingStatus::Tracking && cv::norm(tracked.m_worldVelocity) > 0)
        {
            auto estimatedWorldPosition = tracked.m_worldPosition + tracked.m_worldVelocity;

            cv::Point3f estimatedPosition = convertRealWorldToDepth(estimatedWorldPosition);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(referenceDepth, initialBandwidthDepth, tracked.m_type, iterationMaxTracking);

            recoverTrackingData.matDepth = matDepth;
            recoverTrackingData.matAreaSqrt = areaSqrt;
            recoverTrackingData.matGlobalSegmentation = matSegmentation;
            recoverTrackingData.matScore = matScore;
            recoverTrackingData.matForegroundSearched = foregroundSearched;
            recoverTrackingData.matLayerSegmentation = m_tempLayerSegmentation;
            recoverTrackingData.seedPosition = seedPosition;

            targetPoint = convergeTrackPointFromSeed(recoverTrackingData);
            validateAndUpdateTrackedPoint(matDepth, matArea, tracked, targetPoint);

            if (tracked.m_status == TrackingStatus::Tracking)
            {
                printf("Recovered point %d\n", tracked.m_trackingId);
            }
            else
            {
                /*tracked.m_position = seedPosition;
                tracked.m_worldPosition = estimatedWorldPosition;
                tracked.m_worldVelocity = cv::Point3f();*/
            }
        }
    }

    //remove duplicates
    for (auto iter = m_internalTrackedPoints.begin(); iter != m_internalTrackedPoints.end(); ++iter)
    {
        TrackedPoint& tracked = *iter;
        for (auto otherIter = m_internalTrackedPoints.begin(); otherIter != m_internalTrackedPoints.end(); ++otherIter)
        {
            TrackedPoint& otherTracked = *otherIter;
            bool bothNotDead = tracked.m_status != TrackingStatus::Dead && otherTracked.m_status != TrackingStatus::Dead;
            if (tracked.m_trackingId != otherTracked.m_trackingId && bothNotDead && tracked.m_position == otherTracked.m_position)
            {
                tracked.m_activeFrameCount = MAX(tracked.m_activeFrameCount, otherTracked.m_activeFrameCount);
                tracked.m_inactiveFrameCount = MIN(tracked.m_inactiveFrameCount, otherTracked.m_inactiveFrameCount);
                if (otherTracked.m_type == TrackedPointType::ActivePoint && tracked.m_type != TrackedPointType::ActivePoint)
                {
                    tracked.m_trackingId = otherTracked.m_trackingId;
                    tracked.m_type = TrackedPointType::ActivePoint;
                }
                otherTracked.m_status = TrackingStatus::Dead;
            }
        }
    }

    foregroundSearched = matForeground.clone();

    //add new points (unless already tracking)
    //if (m_internalTrackedPoints.size() == 0)
    while (findForegroundPixel(foregroundSearched, seedPosition))
    {
        //      seedPosition.x = mouseX;
        //      seedPosition.y = mouseY;

        float seedDepth = matDepth.at<float>(seedPosition);
        TrackingData newTrackingData(seedDepth, initialBandwidthDepth, TrackedPointType::CandidatePoint, iterationMaxInitial);

        newTrackingData.matDepth = matDepth;
        newTrackingData.matAreaSqrt = areaSqrt;
        newTrackingData.matGlobalSegmentation = matSegmentation;
        newTrackingData.matScore = matScore;
        newTrackingData.matForegroundSearched = foregroundSearched;
        newTrackingData.matLayerSegmentation = m_tempLayerSegmentation;
        newTrackingData.seedPosition = seedPosition;

        cv::Point targetPoint = convergeTrackPointFromSeed(newTrackingData);

        if (targetPoint.x != -1 && targetPoint.y != -1)
        {
            float area = countNeighborhoodArea(m_tempLayerSegmentation, matDepth, matArea, m_areaBandwidth, m_areaBandwidthDepth, targetPoint);

            if (area > m_minArea && area < m_maxArea)
            {
                bool existingPoint = false;

                for (auto iter = m_internalTrackedPoints.begin(); iter != m_internalTrackedPoints.end(); ++iter)
                {
                    TrackedPoint& tracked = *iter;
                    if (tracked.m_status != TrackingStatus::Dead)
                    {
                        float dist = cv::norm(tracked.m_position - targetPoint);
                        float maxDist = maxMatchDistDefault;
                        if (tracked.m_type == TrackedPointType::ActivePoint && tracked.m_status == TrackingStatus::Lost)
                        {
                            maxDist = maxMatchDistLostActive;
                        }
                        if (dist < maxDist)
                        {
                            tracked.m_inactiveFrameCount = 0;
                            tracked.m_status = TrackingStatus::Tracking;
                            existingPoint = true;
                            break;
                        }
                    }
                }
                if (!existingPoint)
                {
                    float depth = matDepth.at<float>(targetPoint);

                    cv::Point3f worldPosition = convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth);

                    TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId, area);
                    newPoint.m_type = TrackedPointType::CandidatePoint;
                    newPoint.m_status = TrackingStatus::Tracking;
                    ++m_nextTrackingId;
                    m_internalTrackedPoints.push_back(newPoint);
                }
            }
        }
    }

    //filter by area
    //remove old points
    const int maxInactiveFrames = 60;
    const int maxInactiveFramesForLostPoints = 240;
    const int maxInactiveFramesForActivePoints = 480;

    for (auto iter = m_internalTrackedPoints.begin(); iter != m_internalTrackedPoints.end();)
    {
        TrackedPoint& tracked = *iter;

        int max = maxInactiveFrames;
        if (tracked.m_type == TrackedPointType::ActivePoint)
        {
            if (tracked.m_status == TrackingStatus::Lost)
            {
                max = maxInactiveFramesForLostPoints;
            }
            else
            {
                max = maxInactiveFramesForActivePoints;
            }
        }
        //if inactive for more than a certain number of frames, or dead, remove point
        if (tracked.m_inactiveFrameCount > max || tracked.m_status == TrackingStatus::Dead)
        {
            iter = m_internalTrackedPoints.erase(iter);
        }
        else
        {
            ++iter;
        }
    }
}

bool HandTracker::isDepthInRange(const float bandwidth, const float bandwidthDepth, cv::Point& center, float startingDepth, cv::Point3f& position)
{
    cv::Point delta = cv::Point(center.x - position.x, center.y - position.y);
    float len = cv::norm(delta);
    float deltaDepth = abs(position.z - startingDepth);
    bool isValidDepth = len < bandwidth &&
        position.z != 0 &&
        deltaDepth < bandwidthDepth;
    return isValidDepth;
}

void HandTracker::reset()
{
    m_isInitialized = false;
}

std::vector<TrackedPoint>& HandTracker::updateTracking(sensekit_depthframe_t* depthFrame)
{
    int width = depthFrame->width;
    int height = depthFrame->height;

    verifyInit(width, height);

    FrameConverter::depthFrameToMat(depthFrame, m_matDepthOriginal);

    //convert to the target processing size with nearest neighbor
    cv::resize(m_matDepthOriginal, m_matDepth, m_matDepth.size(), 0, 0, CV_INTER_NN);

    //current minus average, scaled by average = velocity as a percent change
    m_matDepthVel = (m_matDepth - m_matDepthAvg) / m_matDepthAvg;

    //accumulate current frame to average using smoothing factor
    cv::accumulateWeighted(m_matDepth, m_matDepthAvg, m_depthSmoothingFactor);

    filterZeroValuesAndJumps(m_matDepth, m_matDepthPrevious, m_matDepthAvg, m_matDepthVel, m_maxDepthJumpPercent);

    //erode to eliminate single pixel velocity artifacts
    erode(abs(m_matDepthVel), m_matDepthVelErode, m_rectElement);

    thresholdForeground(m_matForeground, m_matDepthVelErode, m_foregroundThresholdFactor);

    float minArea = 10000;
    float maxArea = 20000;
    trackPoints(m_matForeground, m_matDepth, m_matHandSegmentation, m_matScore, m_matEdgeDistance, m_matLocalArea);

    if (m_outputSample)
    {
        float sampleDepth = m_matDepth.at<float>(mouseY, mouseX);
        float sampleVelocity = m_matDepthVel.at<float>(mouseY, mouseX);
        float edgeDist = m_matEdgeDistance.at<float>(mouseY, mouseX);
        float area = m_matLocalArea.at<float>(mouseY, mouseX);
        float score = m_matScore.at<float>(mouseY, mouseX);

        printf("(%d, %d) depth: %.0f vel: %.3f score: %.3f edgeDist: %.1f area: %.0f\n", mouseX, mouseY, sampleDepth, sampleVelocity, score, edgeDist, area);
    }

    return updateOriginalPoints(m_internalTrackedPoints);
}

void HandTracker::verifyInit(int width, int height)
{
    float newResizeFactor = width / static_cast<float>(PROCESSING_SIZE_X);

    if (m_isInitialized && m_resizeFactor == newResizeFactor)
    {
        return;
    }

    m_resizeFactor = newResizeFactor;

    m_matDepthOriginal.create(height, width, CV_32FC1);
    m_matDepth.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_matDepthPrevious = cv::Mat::zeros(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_matDepthAvg = cv::Mat::zeros(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_matDepthVel.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_matDepthVelErode.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_matForeground = cv::Mat::zeros(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_8UC1);
    m_matHandSegmentation.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_8UC1);
    m_matScore.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_32FC1);
    m_isInitialized = true;
}

void HandTracker::setupVariables()
{
    m_factor1 = 1;
    m_factor2 = 1;
    m_factor3 = 1;
    m_factor4 = 1;
    m_showCircles = true;

    m_depthSmoothingFactor = 0.05;
    m_velocitySmoothingFactor = 0.05;

    m_maxVelocity = 0.1;
    m_showForeground = false;

    m_foregroundThresholdFactor = 0.02;
    m_maxVelocityFactor = 10;// 0.001;
    m_outputSample = false;
    m_nextTrackingId = 0;

    m_minArea = 5000; //mm^2
    m_maxArea = 20000;  //mm^2
    m_areaBandwidth = 150; //mm
    m_areaBandwidthDepth = 100; //mm

    m_maxDepthJumpPercent = 0.1;

    int erodeNum = 1;
    m_rectElement = cv::getStructuringElement(cv::MORPH_RECT, cv::Size(erodeNum * 2 + 1, erodeNum * 2 + 1), cv::Point(erodeNum, erodeNum));
    m_crossElement = cv::getStructuringElement(cv::MORPH_CROSS, cv::Size(3, 3));
}
