// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif


#include "handtracker.h"
#include "depthutility.h"
#include "segmentationutility.h"
#include "coordinateconversion.h"
#include "pointprocessor.h"
#include <SenseKitUL/streams/hand_types.h>

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

void HandTracker::copyPosition(cv::Point3f& source, sensekit_vector3f_t& target)
{
    target.x = source.x;
    target.y = source.y;
    target.z = source.z;
}

sensekit_handstatus_t HandTracker::convertHandStatus(TrackingStatus status)
{
    switch (status)
    {
    case TrackingStatus::Tracking:
        return sensekit_handstatus_t::HAND_STATUS_NOTTRACKING;
        break;
    case TrackingStatus::Lost:
        return sensekit_handstatus_t::HAND_STATUS_NOTTRACKING;
        break;
    case TrackingStatus::Dead:
    case TrackingStatus::NotTracking:
    default:
        return sensekit_handstatus_t::HAND_STATUS_NOTTRACKING;
        break;
    }
}

void HandTracker::resetHandPoint(sensekit_handpoint_t& point)
{
    point.trackingId = -1;
    point.status = sensekit_handstatus_t::HAND_STATUS_NOTTRACKING;
    point.depthPosition = sensekit_vector2i_t();
    point.worldPosition = sensekit_vector3f_t();
    point.worldDeltaPosition = sensekit_vector3f_t();
}

void HandTracker::updateHandFrame(vector<TrackedPoint>& internalTrackedPoints, sensekit_handframe_wrapper_t* wrapper)
{
    sensekit_handframe_t& frame = wrapper->frame;
    frame.frameIndex++;
    
    int handIndex = 0;
    int maxNumHands = frame.numHands;

    for (auto it = internalTrackedPoints.begin(); it != internalTrackedPoints.end(); ++it)
    {
        TrackedPoint internalPoint = *it;

        TrackingStatus status = internalPoint.m_status;
        if (internalPoint.m_type == TrackedPointType::ActivePoint && status != TrackingStatus::Dead && handIndex < maxNumHands)
        {
            sensekit_handpoint_t& point = frame.handpoints[handIndex];
            ++handIndex;

            point.trackingId = internalPoint.m_trackingId;
            
            //convert from internal depth resolution to original depth resolution
            point.depthPosition.x = internalPoint.m_position.x * m_resizeFactor;
            point.depthPosition.y = internalPoint.m_position.y * m_resizeFactor;

            copyPosition(internalPoint.m_worldPosition, point.worldPosition);
            copyPosition(internalPoint.m_worldDeltaPosition, point.worldDeltaPosition);
            
            point.status = convertHandStatus(status);
        }
    }
    for (int i = handIndex; i < maxNumHands; ++i)
    {
        sensekit_handpoint_t& point = frame.handpoints[i];
        resetHandPoint(point);
    }

    frame.numHands = handIndex;
}

void HandTracker::trackPoints(cv::Mat& matDepth, cv::Mat& matForeground)
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

    cv::Mat matScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    //cv::Mat matEdgeDistance = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    cv::Mat matArea = cv::Mat::zeros(matDepth.size(), CV_32FC1);
    cv::Mat layerSegmentation;

    const float trackingBandwidthDepth = 150;
    const float initialBandwidthDepth = 450;
    const float maxMatchDistLostActive = 500; //mm
    const float maxMatchDistDefault = 200; //mm
    const int iterationMaxTracking = 1;
    const int iterationMaxInitial = 1;

    float heightFactor = 1 * m_factor1;
    float depthFactor = 1.5 * m_factor2;

    SegmentationUtility::calculateBasicScore(matDepth, matScore, heightFactor, depthFactor, m_resizeFactor);
    SegmentationUtility::calculateSegmentArea(matDepth, matArea, m_resizeFactor);

    TrackingMatrices matrices(matDepth, matArea, matScore, matForeground, layerSegmentation);

    m_pointProcessor.updateTrackedPoints(matrices);

    m_pointProcessor.removeDuplicatePoints();

    cv::Mat foregroundSearched = matForeground.clone();

    cv::Point seedPosition;
    //add new points (unless already tracking)
    while (SegmentationUtility::findForegroundPixel(foregroundSearched, seedPosition))
    {
        m_pointProcessor.updateTrackedPointOrCreateNewPointFromSeedPosition(matrices, seedPosition);
    }

    //remove old points
    m_pointProcessor.removeOldOrDeadPoints();
}

void HandTracker::reset()
{
    m_isInitialized = false;
}

void HandTracker::updateTracking(sensekit_depthframe_t* depthFrame)
{
    int width = depthFrame->width;
    int height = depthFrame->height;

    verifyInit(width, height);

    m_depthUtility.processDepthToForeground(depthFrame, m_matDepth, m_matForeground, m_depthSmoothingFactor, m_foregroundThresholdFactor, m_maxDepthJumpPercent);

    float minArea = 10000;
    float maxArea = 20000;
    trackPoints(m_matDepth, m_matForeground);
    
    return updateHandFrame(m_pointProcessor.get_trackedPoints(), m_wrapper);
}

void HandTracker::verifyInit(int width, int height)
{
    float newResizeFactor = width / static_cast<float>(PROCESSING_SIZE_WIDTH);

    if (m_isInitialized && m_resizeFactor == newResizeFactor)
    {
        return;
    }

    m_resizeFactor = newResizeFactor;

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

    m_minArea = 5000; //mm^2
    m_maxArea = 20000;  //mm^2
    m_areaBandwidth = 150; //mm
    m_areaBandwidthDepth = 100; //mm

    m_maxDepthJumpPercent = 0.1;
}
