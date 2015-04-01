#include "trackedpoint.h"
#include "pointprocessor.h"
#include "segmentationutility.h"
#include "coordinateconversion.h"


PointProcessor::PointProcessor()
{
}

PointProcessor::~PointProcessor()
{
}

void PointProcessor::updateTrackedPoints(TrackingMatrices matrices)
{
    for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
    {
        //TODO take this and make it a method on TrackedPoint
        TrackedPoint& trackedPoint = *iter;
        updateTrackedPoint(matrices, trackedPoint);
    }
}

void PointProcessor::updateTrackedPoint(TrackingMatrices matrices, TrackedPoint& trackedPoint)
{
    const float width = matrices.matDepth.cols;
    const float height = matrices.matDepth.rows;

    trackedPoint.m_inactiveFrameCount++;

    cv::Point seedPosition = trackedPoint.m_position;
    float referenceDepth = trackedPoint.m_worldPosition.z;

    TrackingData updateTrackingData(matrices, seedPosition, referenceDepth, trackingBandwidthDepth, trackedPoint.m_type, iterationMaxTracking);

    cv::Point newTargetPoint = SegmentationUtility::convergeTrackPointFromSeed(updateTrackingData);

    validateAndUpdateTrackedPoint(matrices, trackedPoint, newTargetPoint);

    //lost a tracked point, try to guest the position using previous position delta for second chance to recover

    if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldDeltaPosition) > 0)
    {
        auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldDeltaPosition;

        cv::Point3f estimatedPosition = CoordinateConversion::convertRealWorldToDepth(estimatedWorldPosition, resizeFactor);

        seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
        seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
        referenceDepth = estimatedPosition.z;

        TrackingData recoverTrackingData(matrices, seedPosition, referenceDepth, initialBandwidthDepth, trackedPoint.m_type, iterationMaxTracking);

        newTargetPoint = SegmentationUtility::convergeTrackPointFromSeed(recoverTrackingData);
        validateAndUpdateTrackedPoint(matrices, trackedPoint, newTargetPoint);

        if (trackedPoint.m_status == TrackingStatus::Tracking)
        {
            printf("Recovered point %d\n", trackedPoint.m_trackingId);
        }
        else
        {
            /*tracked.m_position = seedPosition;
            tracked.m_worldPosition = estimatedWorldPosition;
            tracked.m_worldDeltaPosition = cv::Point3f();*/
        }
    }
}


void PointProcessor::validateAndUpdateTrackedPoint(TrackingMatrices matrices, TrackedPoint& trackedPoint, cv::Point newTargetPoint)
{
    bool updatedPoint = false;
    const float steadyDist = 150; //mm
    const float maxJumpDist = 450; //mm

    if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
    {
        float depth = matrices.matDepth.at<float>(newTargetPoint);

        cv::Point3f worldPosition = CoordinateConversion::convertDepthToRealWorld(newTargetPoint.x, newTargetPoint.y, depth, resizeFactor);

        auto dist = cv::norm(worldPosition - trackedPoint.m_worldPosition);
        auto deadbandDist = cv::norm(worldPosition - trackedPoint.m_steadyWorldPosition);

        float area = SegmentationUtility::countNeighborhoodArea(matrices.matLayerSegmentation, matrices.matDepth, matrices.matArea, newTargetPoint, areaBandwidth, areaBandwidthDepth, resizeFactor);

        if (dist < maxJumpDist && area > minArea && area < maxArea)
        {
            updatedPoint = true;
            cv::Point3f deltaPosition = worldPosition - trackedPoint.m_worldPosition;
            trackedPoint.m_worldPosition = worldPosition;
            trackedPoint.m_worldDeltaPosition = deltaPosition;

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


bool PointProcessor::isValidPointArea(TrackingMatrices& matrices, cv::Point targetPoint)
{
    bool validPointArea = false;
    if (targetPoint.x != -1 && targetPoint.y != -1)
    {
        float area = SegmentationUtility::countNeighborhoodArea(matrices.matLayerSegmentation, matrices.matDepth, matrices.matArea, targetPoint, areaBandwidth, areaBandwidthDepth, resizeFactor);

        if (area > minArea && area < maxArea)
        {
            validPointArea = true;
        }
    }
    return validPointArea;
}

void PointProcessor::removeDuplicatePoints()
{
    for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
    {
        TrackedPoint& tracked = *iter;
        for (auto otherIter = m_trackedPoints.begin(); otherIter != m_trackedPoints.end(); ++otherIter)
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

void PointProcessor::removeOldOrDeadPoints()
{
    const int maxInactiveFrames = 60;
    const int maxInactiveFramesForLostPoints = 240;
    const int maxInactiveFramesForActivePoints = 480;

    for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end();)
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
            iter = m_trackedPoints.erase(iter);
        }
        else
        {
            ++iter;
        }
    }
}

void PointProcessor::updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices matrices, cv::Point seedPosition)
{
    float seedDepth = matrices.matDepth.at<float>(seedPosition);
    TrackingData trackingData(matrices, seedPosition, seedDepth, initialBandwidthDepth, TrackedPointType::CandidatePoint, iterationMaxInitial);

    cv::Point targetPoint = SegmentationUtility::convergeTrackPointFromSeed(trackingData);

    bool validPointArea = isValidPointArea(matrices, targetPoint);

    if (validPointArea)
    {
        bool existingPoint = false;

        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
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
            float depth = matrices.matDepth.at<float>(targetPoint);

            cv::Point3f worldPosition = CoordinateConversion::convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth, resizeFactor);

            TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId);
            newPoint.m_type = TrackedPointType::CandidatePoint;
            newPoint.m_status = TrackingStatus::Tracking;
            ++m_nextTrackingId;
            m_trackedPoints.push_back(newPoint);
        }
    }
}
