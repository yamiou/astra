#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "SegmentationUtility.h"
#include "CoordinateConverter.h"

namespace sensekit { namespace plugins { namespace hands {

    PointProcessor::PointProcessor(const CoordinateConverter& converter) :
        m_converter(converter),
        m_trackingBandwidthDepth(150),  //mm
        m_initialBandwidthDepth(450),   //mm
        m_maxMatchDistLostActive(500),  //mm
        m_maxMatchDistDefault(200),     //mm
        m_iterationMaxInitial(1),
        m_iterationMaxTracking(1),
        m_minArea(5000),            //mm^2
        m_maxArea(20000),           //mm^2
        m_areaBandwidth(150),       //mm
        m_areaBandwidthDepth(100)   //mm
    {
    }

    PointProcessor::~PointProcessor()
    {
    }

    void PointProcessor::updateTrackedPoints(TrackingMatrices& matrices)
    {
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            //TODO take this and make it a method on TrackedPoint
            TrackedPoint& trackedPoint = *iter;
            updateTrackedPoint(matrices, trackedPoint);
        }
    }

    void PointProcessor::reset()
    {
        m_trackedPoints.clear();
        m_nextTrackingId = 0;
    }

    void PointProcessor::updateTrackedPoint(TrackingMatrices& matrices, TrackedPoint& trackedPoint)
    {
        const float width = matrices.matDepth.cols;
        const float height = matrices.matDepth.rows;

        trackedPoint.m_inactiveFrameCount++;

        cv::Point seedPosition = trackedPoint.m_position;
        float referenceDepth = trackedPoint.m_worldPosition.z;

        TrackingData updateTrackingData(matrices, seedPosition, referenceDepth, m_trackingBandwidthDepth, trackedPoint.m_type, m_iterationMaxTracking);

        cv::Point newTargetPoint = SegmentationUtility::convergeTrackPointFromSeed(updateTrackingData);

        validateAndUpdateTrackedPoint(matrices, trackedPoint, newTargetPoint);

        //lost a tracked point, try to guess the position using previous position delta for second chance to recover

        if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldDeltaPosition) > 0)
        {
            auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldDeltaPosition;

            cv::Point3f estimatedPosition = m_converter.convertRealWorldToDepth(estimatedWorldPosition);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(matrices, seedPosition, referenceDepth, m_initialBandwidthDepth, trackedPoint.m_type, m_iterationMaxTracking);

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


    void PointProcessor::validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                                       TrackedPoint& trackedPoint,
                                                       const cv::Point& newTargetPoint)
    {
        bool updatedPoint = false;
        const float steadyDist = 150; //mm
        const float maxJumpDist = 450; //mm

        if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
        {
            float depth = matrices.matDepth.at<float>(newTargetPoint);

            cv::Point3f worldPosition = m_converter.convertDepthToRealWorld(newTargetPoint.x, newTargetPoint.y, depth);

            auto dist = cv::norm(worldPosition - trackedPoint.m_worldPosition);
            auto deadbandDist = cv::norm(worldPosition - trackedPoint.m_steadyWorldPosition);

            float area = SegmentationUtility::countNeighborhoodArea(matrices.matLayerSegmentation, matrices.matDepth, matrices.matArea, newTargetPoint, m_areaBandwidth, m_areaBandwidthDepth, m_converter);

            if (dist < maxJumpDist && area > m_minArea && area < m_maxArea)
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
            float area = SegmentationUtility::countNeighborhoodArea(matrices.matLayerSegmentation, matrices.matDepth, matrices.matArea, targetPoint, m_areaBandwidth, m_areaBandwidthDepth, m_converter);

            if (area > m_minArea && area < m_maxArea)
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

    void PointProcessor::updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                            const cv::Point& seedPosition)
    {
        float seedDepth = matrices.matDepth.at<float>(seedPosition);
        TrackingData trackingData(matrices, seedPosition, seedDepth, m_initialBandwidthDepth, TrackedPointType::CandidatePoint, m_iterationMaxInitial);

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
                    float maxDist = m_maxMatchDistDefault;
                    if (tracked.m_type == TrackedPointType::ActivePoint && tracked.m_status == TrackingStatus::Lost)
                    {
                        maxDist = m_maxMatchDistLostActive;
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

                cv::Point3f worldPosition = m_converter.convertDepthToRealWorld(targetPoint.x, targetPoint.y, depth);

                TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId);
                newPoint.m_type = TrackedPointType::CandidatePoint;
                newPoint.m_status = TrackingStatus::Tracking;
                ++m_nextTrackingId;
                m_trackedPoints.push_back(newPoint);
            }
        }
    }
}}}
