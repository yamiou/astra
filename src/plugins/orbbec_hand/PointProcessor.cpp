#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "Segmentation.h"

namespace sensekit { namespace plugins { namespace hand {

    PointProcessor::PointProcessor(const ScalingCoordinateMapper& mapper) :
        m_mapper(mapper),
        m_trackingBandwidthDepth(150),  //mm
        m_initialBandwidthDepth(450),   //mm
        m_maxMatchDistLostActive(500),  //mm
        m_maxMatchDistDefault(200),     //mm
        m_iterationMaxInitial(1),
        m_iterationMaxTracking(1),
        m_minArea(5000),                //mm^2
        m_maxArea(25000),               //mm^2
        m_areaBandwidth(150),           //mm
        m_areaBandwidthDepth(200),      //mm
        m_maxSegmentationDist(250),     //mm
        m_steadyDeadBandRadius(150),    //mm
        m_maxJumpDist(450),             //mm
        m_targetEdgeDistance(60),       //mm
        m_edgeDistanceFactor(10),
        m_maxInactiveFramesToBeConsideredActive(10),
        m_minActiveFramesToLockTracking(60),
        m_maxInactiveFramesForCandidatePoints(60),
        m_maxInactiveFramesForLostPoints(240),
        m_maxInactiveFramesForActivePoints(480)
    {}

    PointProcessor::~PointProcessor()
    {}

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

    float PointProcessor::get_point_area(TrackingMatrices& matrices, const cv::Point& point)
    {
        float area = segmentation::count_neighborhood_area(matrices.layerSegmentation,
                                                           matrices.depth,
                                                           matrices.area,
                                                           point,
                                                           m_areaBandwidth,
                                                           m_areaBandwidthDepth,
                                                           m_mapper);

        return area;
    }

    bool PointProcessor::is_valid_point_area(TrackingMatrices& matrices, const cv::Point& targetPoint)
    {
        float area = get_point_area(matrices, targetPoint);

        bool validPointArea = area > m_minArea && area < m_maxArea;
        return validPointArea;
    }

    cv::Point PointProcessor::adjustPointForEdge(TrackingMatrices& matrices, const cv::Point& rawTargetPoint)
    {
        cv::Size size = matrices.depth.size();
        matrices.layerEdgeDistance = cv::Mat::zeros(size, CV_32FC1);
        matrices.layerScore = cv::Mat::zeros(size, CV_32FC1);

        segmentation::calculate_edge_distance(matrices.layerSegmentation,
                                              matrices.areaSqrt,
                                              matrices.layerEdgeDistance);

        segmentation::calculate_layer_score(matrices.depth,
                                            matrices.basicScore,
                                            matrices.layerScore,
                                            matrices.layerEdgeDistance,
                                            m_edgeDistanceFactor,
                                            m_targetEdgeDistance);

        double min, max;
        cv::Point minLoc, maxLoc;

        cv::minMaxLoc(matrices.layerScore, &min, &max, &minLoc, &maxLoc, matrices.layerSegmentation);

        return maxLoc;
    }

    void PointProcessor::updateTrackedPoint(TrackingMatrices& matrices, TrackedPoint& trackedPoint)
    {
        const float width = matrices.depth.cols;
        const float height = matrices.depth.rows;

        trackedPoint.m_inactiveFrameCount++;

        cv::Point seedPosition = trackedPoint.m_position;
        float referenceDepth = trackedPoint.m_worldPosition.z;

        TrackingData updateTrackingData(matrices, 
                                        seedPosition, 
                                        referenceDepth, 
                                        m_trackingBandwidthDepth, 
                                        trackedPoint.m_type, 
                                        m_iterationMaxTracking,
                                        m_maxSegmentationDist,
                                        FG_POLICY_IGNORE);

        cv::Point rawTargetPoint = segmentation::converge_track_point_from_seed(updateTrackingData);
        
        cv::Point newTargetPoint = adjustPointForEdge(matrices, rawTargetPoint);

        validateAndUpdateTrackedPoint(matrices, trackedPoint, newTargetPoint);

        //lost a tracked point, try to guess the position using previous position delta for second chance to recover

        if (trackedPoint.m_status != TrackingStatus::Tracking && cv::norm(trackedPoint.m_worldDeltaPosition) > 0)
        {
            auto estimatedWorldPosition = trackedPoint.m_worldPosition + trackedPoint.m_worldDeltaPosition;

            cv::Point3f estimatedPosition = m_mapper.convert_world_to_depth(estimatedWorldPosition);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(matrices, 
                                             seedPosition, 
                                             referenceDepth, 
                                             m_initialBandwidthDepth, 
                                             trackedPoint.m_type, 
                                             m_iterationMaxTracking,
                                             m_maxSegmentationDist,
                                             FG_POLICY_IGNORE);

            rawTargetPoint = segmentation::converge_track_point_from_seed(recoverTrackingData);

            newTargetPoint = adjustPointForEdge(matrices, rawTargetPoint);

            validateAndUpdateTrackedPoint(matrices, trackedPoint, newTargetPoint);

            if (trackedPoint.m_status == TrackingStatus::Tracking)
            {
                printf("Recovered point %d\n", trackedPoint.m_trackingId);
            }
        }
    }


    void PointProcessor::validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                                       TrackedPoint& trackedPoint,
                                                       const cv::Point& newTargetPoint)
    {
        bool updatedPoint = false;
        
        if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
        {
            float depth = matrices.depth.at<float>(newTargetPoint);

            cv::Point3f worldPosition = m_mapper.convert_depth_to_world(newTargetPoint.x, newTargetPoint.y, depth);

            auto dist = cv::norm(worldPosition - trackedPoint.m_worldPosition);
            auto steadyDist = cv::norm(worldPosition - trackedPoint.m_steadyWorldPosition);

            bool validArea = is_valid_point_area(matrices, newTargetPoint);

            if (dist < m_maxJumpDist && validArea)
            {
                updatedPoint = true;
                cv::Point3f deltaPosition = worldPosition - trackedPoint.m_worldPosition;
                trackedPoint.m_worldPosition = worldPosition;
                trackedPoint.m_worldDeltaPosition = deltaPosition;

                trackedPoint.m_position = newTargetPoint;
                if (steadyDist > m_steadyDeadBandRadius)
                {
                    trackedPoint.m_steadyWorldPosition = worldPosition;
                    trackedPoint.m_inactiveFrameCount = 0;
                }

                if (trackedPoint.m_inactiveFrameCount < m_maxInactiveFramesToBeConsideredActive)
                {
                    trackedPoint.m_activeFrameCount++;
                    if (trackedPoint.m_activeFrameCount > m_minActiveFramesToLockTracking)
                    {
                        trackedPoint.m_type = TrackedPointType::ActivePoint;
                    }
                }
            }
        }

        assert(trackedPoint.m_status != TrackingStatus::Dead);

        if (updatedPoint)
        {
            trackedPoint.m_status = TrackingStatus::Tracking;
        }
        else
        {
            trackedPoint.m_status = TrackingStatus::Lost;
        }
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
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end();)
        {
            TrackedPoint& tracked = *iter;

            int max = m_maxInactiveFramesForCandidatePoints;
            if (tracked.m_type == TrackedPointType::ActivePoint)
            {
                if (tracked.m_status == TrackingStatus::Lost)
                {
                    max = m_maxInactiveFramesForLostPoints;
                }
                else
                {
                    max = m_maxInactiveFramesForActivePoints;
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
        float referenceDepth = matrices.depth.at<float>(seedPosition);
        if (referenceDepth == 0)
        {
            //Cannot expect to properly segment when the seedPosition has zero depth
            return;
        }
        TrackingData trackingData(matrices, 
                                  seedPosition, 
                                  referenceDepth, 
                                  m_initialBandwidthDepth, 
                                  TrackedPointType::CandidatePoint, 
                                  m_iterationMaxInitial,
                                  m_maxSegmentationDist,
                                  FG_POLICY_RESET_TTL);

        cv::Point rawTargetPoint = segmentation::converge_track_point_from_seed(trackingData);

        cv::Point targetPoint = adjustPointForEdge(matrices, rawTargetPoint);

        bool validPointArea = is_valid_point_area(matrices, targetPoint);

        if (targetPoint.x != -1 && targetPoint.y != -1 && validPointArea)
        {
            bool existingPoint = false;
            
            float depth = matrices.depth.at<float>(targetPoint);

            cv::Point3f worldPosition = m_mapper.convert_depth_to_world(targetPoint.x, 
                                                                                   targetPoint.y, 
                                                                                   depth);

            for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
            {
                TrackedPoint& tracked = *iter;
                if (tracked.m_status != TrackingStatus::Dead)
                {
                    float dist = cv::norm(tracked.m_worldPosition - worldPosition);
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
                TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId);
                newPoint.m_type = TrackedPointType::CandidatePoint;
                newPoint.m_status = TrackingStatus::Tracking;
                ++m_nextTrackingId;
                m_trackedPoints.push_back(newPoint);
            }
        }
    }
}}}
