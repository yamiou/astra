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
#include "segmentationtracker.h"

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

cv::Point3f HandTracker::convertDepthToRealWorld(float localX, float localY, float localZ, const float resizeFactor)
{
    float worldX, worldY, worldZ;

    localX *= resizeFactor;
    localY *= resizeFactor;

    convert_depth_to_world(localX, localY, localZ, &worldX, &worldY, &worldZ);

    return cv::Point3f(worldX, worldY, worldZ);
}

cv::Point3f HandTracker::convertDepthToRealWorld(cv::Point3f localPosition, const float resizeFactor)
{
    return convertDepthToRealWorld(localPosition.x, localPosition.y, localPosition.z, resizeFactor);
}

cv::Point3f HandTracker::convertRealWorldToDepth(cv::Point3f worldPosition, const float resizeFactor)
{
    float localX, localY, localZ;

    convert_world_to_depth(worldPosition.x, worldPosition.y, worldPosition.z, &localX, &localY, &localZ);

    localX /= resizeFactor;
    localY /= resizeFactor;

    return cv::Point3f(localX, localY, localZ);
}

cv::Point HandTracker::offsetPixelLocationByMM(cv::Point& position, float offsetX, float offsetY, float depth, const float resizeFactor)
{
    cv::Point3f world = convertDepthToRealWorld(position.x, position.y, depth, resizeFactor);

    world.x += offsetX;
    world.y += offsetY;

    cv::Point3f offsetLocal = convertRealWorldToDepth(world, resizeFactor);

    return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
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

void HandTracker::calculateBasicScore(cv::Mat& matDepth, cv::Mat& matScore, const float heightFactor, const float depthFactor, const float resizeFactor)
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
                cv::Point3f worldPosition = convertDepthToRealWorld(x, y, depth, resizeFactor);

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
    const int maxIterations = PROCESSING_SIZE_X / 2;
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

void HandTracker::calculateSegmentArea(cv::Mat& matDepth, cv::Mat& matArea, const float resizeFactor)
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

                area += getDepthArea(p1, p2, p3, resizeFactor);
                area += getDepthArea(p2, p3, p4, resizeFactor);
            }

            *areaRow = area;

            ++depthRow;
            ++nextDepthRow;
            ++areaRow;
        }
    }
}

float HandTracker::getDepthArea(cv::Point3f& p1, cv::Point3f& p2, cv::Point3f& p3, const float resizeFactor)
{
    float worldX1, worldY1, worldZ1;
    float worldX2, worldY2, worldZ2;
    float worldX3, worldY3, worldZ3;

    cv::Point3f world1 = convertDepthToRealWorld(p1, resizeFactor);
    cv::Point3f world2 = convertDepthToRealWorld(p2, resizeFactor);
    cv::Point3f world3 = convertDepthToRealWorld(p3, resizeFactor);

    cv::Point3f v1 = world2 - world1;
    cv::Point3f v2 = world3 - world1;

    float area = 0.5 * cv::norm(v1.cross(v2));
    return area;
}

float HandTracker::countNeighborhoodArea(cv::Mat& matSegmentation, cv::Mat& matDepth, cv::Mat& matArea, cv::Point center, const float bandwidth, const float bandwidthDepth, const float resizeFactor)
{
    float startingDepth = matDepth.at<float>(center);

    cv::Point topLeft = offsetPixelLocationByMM(center, -bandwidth, bandwidth, startingDepth, resizeFactor);
    cv::Point bottomRight = offsetPixelLocationByMM(center, bandwidth, -bandwidth, startingDepth, resizeFactor);
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

void HandTracker::validateAndUpdateTrackedPoint(cv::Mat& matDepth, cv::Mat& matArea, cv::Mat& matLayerSegmentation, TrackedPoint& trackedPoint, cv::Point newTargetPoint, const float resizeFactor, const float minArea, const float maxArea, const float areaBandwidth, const float areaBandwidthDepth)
{
    bool updatedPoint = false;
    const float steadyDist = 150; //mm
    const float maxJumpDist = 450; //mm

    if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
    {
        float depth = matDepth.at<float>(newTargetPoint);

        cv::Point3f worldPosition = convertDepthToRealWorld(newTargetPoint.x, newTargetPoint.y, depth, resizeFactor);

        auto dist = cv::norm(worldPosition - trackedPoint.m_worldPosition);
        auto deadbandDist = cv::norm(worldPosition - trackedPoint.m_steadyWorldPosition);

        float area = countNeighborhoodArea(matLayerSegmentation, matDepth, matArea, newTargetPoint, areaBandwidth, areaBandwidthDepth, resizeFactor);

        if (dist < maxJumpDist && area > minArea && area < maxArea)
        {
            updatedPoint = true;
            cv::Point3f worldVelocity = worldPosition - trackedPoint.m_worldPosition;
            trackedPoint.m_worldPosition = worldPosition;
            trackedPoint.m_worldVelocity = worldVelocity;

            trackedPoint.m_position = newTargetPoint;
            if (deadbandDist > steadyDist)
            {
                trackedPoint.m_steadyWorldPosition = worldPosition;
                trackedPoint.m_inactiveFrameCount = 0;
            }

            if (trackedPoint.m_inactiveFrameCount < 10)
            {
                trackedPoint.m_activeFrameCount++;
                if (trackedPoint.m_activeFrameCount > 120)
                {
                    trackedPoint.m_type = TrackedPointType::ActivePoint;
                }
            }
        }
    }

    if (trackedPoint.m_status != TrackingStatus::Dead)
    {
        if (updatedPoint)
        {
            trackedPoint.m_status = TrackingStatus::Tracking;
        }
        else
        {
            trackedPoint.m_status = TrackingStatus::Lost;
        }
    }
}

void HandTracker::removeDuplicatePoints(vector<TrackedPoint>& trackedPoints)
{
    for (auto iter = trackedPoints.begin(); iter != trackedPoints.end(); ++iter)
    {
        TrackedPoint& tracked = *iter;
        for (auto otherIter = trackedPoints.begin(); otherIter != trackedPoints.end(); ++otherIter)
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
}

void HandTracker::removeOldAndDeadPoints(vector<TrackedPoint>& trackedPoints)
{
    const int maxInactiveFrames = 60;
    const int maxInactiveFramesForLostPoints = 240;
    const int maxInactiveFramesForActivePoints = 480;

    for (auto iter = trackedPoints.begin(); iter != trackedPoints.end();)
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
            iter = trackedPoints.erase(iter);
        }
        else
        {
            ++iter;
        }
    }
}

void HandTracker::trackPoints(cv::Mat& matForeground, cv::Mat& matDepth, cv::Mat& matGlobalSegmentation, cv::Mat& matScore, cv::Mat& matEdgeDistance, cv::Mat& matArea)
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

    matGlobalSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
    matScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    matEdgeDistance = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    matArea = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    
    const float trackingBandwidthDepth = 150;
    const float initialBandwidthDepth = 450;
    const float width = matDepth.cols;
    const float height = matDepth.rows;
    const float maxMatchDistLostActive = 500; //mm
    const float maxMatchDistDefault = 200; //mm
    const int iterationMaxTracking = 1;
    const int iterationMaxInitial = 1;


    float heightFactor = 1 * m_factor1;
    float depthFactor = 1.5 * m_factor2;

    calculateBasicScore(matDepth, matScore, heightFactor, depthFactor, m_resizeFactor);
    calculateSegmentArea(matDepth, matArea, m_resizeFactor);

    //update existing points
    for (auto iter = m_internalTrackedPoints.begin(); iter != m_internalTrackedPoints.end(); ++iter)
    {
        TrackedPoint& trackedPoint = *iter;
        trackedPoint.m_inactiveFrameCount++;
        seedPosition = trackedPoint.m_position;
        float referenceDepth = trackedPoint.m_worldPosition.z;

        TrackingData updateTrackingData(referenceDepth, trackingBandwidthDepth, trackedPoint.m_type, iterationMaxTracking);

        updateTrackingData.matDepth = matDepth;
        updateTrackingData.matArea = matArea;
        updateTrackingData.matGlobalSegmentation = matGlobalSegmentation;
        updateTrackingData.matScore = matScore;
        updateTrackingData.matForegroundSearched = foregroundSearched;
        updateTrackingData.matLayerSegmentation = m_layerSegmentation;
        updateTrackingData.seedPosition = seedPosition;

        cv::Point newTargetPoint = SegmentationTracker::convergeTrackPointFromSeed(updateTrackingData);

        validateAndUpdateTrackedPoint(matDepth, matArea, m_layerSegmentation, trackedPoint, newTargetPoint, m_resizeFactor, m_minArea, m_maxArea, m_areaBandwidth, m_areaBandwidthDepth);

        //lost a tracked point, try to guest the position using previous velocity for second chance to recover
        if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldVelocity) > 0)
        {
            auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldVelocity;

            cv::Point3f estimatedPosition = convertRealWorldToDepth(estimatedWorldPosition, m_resizeFactor);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(referenceDepth, initialBandwidthDepth, trackedPoint.m_type, iterationMaxTracking);

            recoverTrackingData.matDepth = matDepth;
            recoverTrackingData.matArea = matArea;
            recoverTrackingData.matGlobalSegmentation = matGlobalSegmentation;
            recoverTrackingData.matScore = matScore;
            recoverTrackingData.matForegroundSearched = foregroundSearched;
            recoverTrackingData.matLayerSegmentation = m_layerSegmentation;
            recoverTrackingData.seedPosition = seedPosition;

            newTargetPoint = SegmentationTracker::convergeTrackPointFromSeed(recoverTrackingData);
            validateAndUpdateTrackedPoint(matDepth, matArea, m_layerSegmentation, trackedPoint, newTargetPoint, m_resizeFactor, m_minArea, m_maxArea, m_areaBandwidth, m_areaBandwidthDepth);

            if (trackedPoint.m_status == TrackingStatus::Tracking)
            {
                printf("Recovered point %d\n", trackedPoint.m_trackingId);
            }
            else
            {
                /*tracked.m_position = seedPosition;
                tracked.m_worldPosition = estimatedWorldPosition;
                tracked.m_worldVelocity = cv::Point3f();*/
            }
        }
    }

    removeDuplicatePoints(m_internalTrackedPoints);

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
        newTrackingData.matArea = matArea;
        newTrackingData.matGlobalSegmentation = matGlobalSegmentation;
        newTrackingData.matScore = matScore;
        newTrackingData.matForegroundSearched = foregroundSearched;
        newTrackingData.matLayerSegmentation = m_layerSegmentation;
        newTrackingData.seedPosition = seedPosition;

        cv::Point targetPoint = SegmentationTracker::convergeTrackPointFromSeed(newTrackingData);

        if (targetPoint.x != -1 && targetPoint.y != -1)
        {
            float area = countNeighborhoodArea(m_layerSegmentation, matDepth, matArea, targetPoint, m_areaBandwidth, m_areaBandwidthDepth, m_resizeFactor);

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

                    cv::Point3f worldPosition = convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth, m_resizeFactor);

                    TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId, area);
                    newPoint.m_type = TrackedPointType::CandidatePoint;
                    newPoint.m_status = TrackingStatus::Tracking;
                    ++m_nextTrackingId;
                    m_internalTrackedPoints.push_back(newPoint);
                }
            }
        }
    }

    
    //remove old points
    removeOldAndDeadPoints(m_internalTrackedPoints);
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
    trackPoints(m_matForeground, m_matDepth, m_matGlobalSegmentation, m_matScore, m_matEdgeDistance, m_matLocalArea);

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
    m_matGlobalSegmentation.create(PROCESSING_SIZE_Y, PROCESSING_SIZE_X, CV_8UC1);
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
}
