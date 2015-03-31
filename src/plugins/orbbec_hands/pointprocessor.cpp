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

    //lost a tracked point, try to guest the position using previous velocity for second chance to recover

    if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldVelocity) > 0)
    {
        auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldVelocity;

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
            tracked.m_worldVelocity = cv::Point3f();*/
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


void PointProcessor::updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices matrices, std::vector<TrackedPoint>& trackedPoints, cv::Point seedPosition, int& nextTrackingId)
{
    float seedDepth = matrices.matDepth.at<float>(seedPosition);
    TrackingData trackingData(matrices, seedPosition, seedDepth, initialBandwidthDepth, TrackedPointType::CandidatePoint, iterationMaxInitial);

    cv::Point targetPoint = SegmentationUtility::convergeTrackPointFromSeed(trackingData);

    bool validPointArea = false;
    if (targetPoint.x != -1 && targetPoint.y != -1)
    {
        float area = SegmentationUtility::countNeighborhoodArea(matrices.matLayerSegmentation, matrices.matDepth, matrices.matArea, targetPoint, areaBandwidth, areaBandwidthDepth, resizeFactor);

        if (area > minArea && area < maxArea)
        {
            validPointArea = true;
        }
    }

    if (validPointArea)
    {
        bool existingPoint = false;

        for (auto iter = trackedPoints.begin(); iter != trackedPoints.end(); ++iter)
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

            TrackedPoint newPoint(targetPoint, worldPosition, nextTrackingId);
            newPoint.m_type = TrackedPointType::CandidatePoint;
            newPoint.m_status = TrackingStatus::Tracking;
            ++nextTrackingId;
            trackedPoints.push_back(newPoint);
        }
    }
}
