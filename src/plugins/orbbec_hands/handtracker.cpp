// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif


#include "handtracker.h"
#include <math.h>

#include <cstdint>
#include <queue>
#include <ctime>
#include "depthutility.h"
#include "segmentationutility.h"
#include "coordinateconversion.h"

using namespace std;

#define GL_WIN_SIZE_X   640
#define GL_WIN_SIZE_Y   480

#define PROCESSING_SIZE_WIDTH 80
#define PROCESSING_SIZE_HEIGHT 60

#define TEXTURE_SIZE    512

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))

HandTracker::HandTracker(sensekit_depthstream_t* depthStream) :
m_depthStream(depthStream),
m_depthUtility(PROCESSING_SIZE_WIDTH, PROCESSING_SIZE_HEIGHT)
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

void HandTracker::validateAndUpdateTrackedPoint(cv::Mat& matDepth, cv::Mat& matArea, cv::Mat& matLayerSegmentation, TrackedPoint& trackedPoint, cv::Point newTargetPoint, const float resizeFactor, const float minArea, const float maxArea, const float areaBandwidth, const float areaBandwidthDepth)
{
    bool updatedPoint = false;
    const float steadyDist = 150; //mm
    const float maxJumpDist = 450; //mm

    if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
    {
        float depth = matDepth.at<float>(newTargetPoint);

        cv::Point3f worldPosition = CoordinateConversion::convertDepthToRealWorld(newTargetPoint.x, newTargetPoint.y, depth, resizeFactor);

        auto dist = cv::norm(worldPosition - trackedPoint.m_worldPosition);
        auto deadbandDist = cv::norm(worldPosition - trackedPoint.m_steadyWorldPosition);

        float area = SegmentationUtility::countNeighborhoodArea(matLayerSegmentation, matDepth, matArea, newTargetPoint, areaBandwidth, areaBandwidthDepth, resizeFactor);

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

void HandTracker::removeOldOrDeadPoints(vector<TrackedPoint>& trackedPoints)
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

    SegmentationUtility::calculateBasicScore(matDepth, matScore, heightFactor, depthFactor, m_resizeFactor);
    SegmentationUtility::calculateSegmentArea(matDepth, matArea, m_resizeFactor);

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

        cv::Point newTargetPoint = SegmentationUtility::convergeTrackPointFromSeed(updateTrackingData);

        validateAndUpdateTrackedPoint(matDepth, matArea, m_layerSegmentation, trackedPoint, newTargetPoint, m_resizeFactor, m_minArea, m_maxArea, m_areaBandwidth, m_areaBandwidthDepth);

        //lost a tracked point, try to guest the position using previous velocity for second chance to recover
        if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldVelocity) > 0)
        {
            auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldVelocity;

            cv::Point3f estimatedPosition = CoordinateConversion::convertRealWorldToDepth(estimatedWorldPosition, m_resizeFactor);

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

            newTargetPoint = SegmentationUtility::convergeTrackPointFromSeed(recoverTrackingData);
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
    while (SegmentationUtility::findForegroundPixel(foregroundSearched, seedPosition))
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

        cv::Point targetPoint = SegmentationUtility::convergeTrackPointFromSeed(newTrackingData);

        if (targetPoint.x != -1 && targetPoint.y != -1)
        {
            float area = SegmentationUtility::countNeighborhoodArea(m_layerSegmentation, matDepth, matArea, targetPoint, m_areaBandwidth, m_areaBandwidthDepth, m_resizeFactor);

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

                    cv::Point3f worldPosition = CoordinateConversion::convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth, m_resizeFactor);

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
    removeOldOrDeadPoints(m_internalTrackedPoints);
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

    m_depthUtility.processDepthToForeground(depthFrame, m_matDepth, m_matForeground, m_depthSmoothingFactor, m_foregroundThresholdFactor, m_maxDepthJumpPercent);

    float minArea = 10000;
    float maxArea = 20000;
    trackPoints(m_matForeground, m_matDepth, m_matGlobalSegmentation, m_matScore, m_matEdgeDistance, m_matLocalArea);

    if (m_outputSample)
    {
        float sampleDepth = m_matDepth.at<float>(mouseY, mouseX);
        float edgeDist = m_matEdgeDistance.at<float>(mouseY, mouseX);
        float area = m_matLocalArea.at<float>(mouseY, mouseX);
        float score = m_matScore.at<float>(mouseY, mouseX);

        printf("(%d, %d) depth: %.0f score: %.3f edgeDist: %.1f area: %.0f\n", mouseX, mouseY, sampleDepth, score, edgeDist, area);
    }

    return updateOriginalPoints(m_internalTrackedPoints);
}

void HandTracker::verifyInit(int width, int height)
{
    float newResizeFactor = width / static_cast<float>(PROCESSING_SIZE_WIDTH);

    if (m_isInitialized && m_resizeFactor == newResizeFactor)
    {
        return;
    }

    m_resizeFactor = newResizeFactor;

    m_matGlobalSegmentation.create(PROCESSING_SIZE_HEIGHT, PROCESSING_SIZE_WIDTH, CV_8UC1);
    m_matScore.create(PROCESSING_SIZE_HEIGHT, PROCESSING_SIZE_WIDTH, CV_32FC1);

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
}
