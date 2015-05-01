#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "Segmentation.h"

namespace sensekit { namespace plugins { namespace hand {

    PointProcessor::PointProcessor() :
        m_updateCycleBandwidthDepth(150),  //mm
        m_createCycleBandwidthDepth(450),   //mm
        m_maxMatchDistLostActive(500),  //mm
        m_maxMatchDistDefault(200),     //mm
        m_iterationMaxInitial(1),
        m_iterationMaxTracking(1),
        m_iterationMaxRefinement(1),
        m_minArea(0),                //mm^2
        m_maxArea(25000),               //mm^2
        m_areaBandwidth(250),           //mm
        m_areaBandwidthDepth(100),      //mm
        m_maxSegmentationDist(250),     //mm
        m_steadyDeadBandRadius(150),    //mm
        m_maxJumpDist(250),             //mm
        m_targetEdgeDistance(40),       //mm
        m_heightScoreFactor(0.5),
        m_depthScoreFactor(2.0),
        m_edgeDistanceScoreFactor(4.0),
        m_pointInertiaFactor(50.0),
        m_pointInertiaRadius(60),       //mm
        m_maxInactiveFramesToBeConsideredActive(10),
        m_minActiveFramesToLockTracking(60),
        m_maxInactiveFramesForCandidatePoints(60),
        m_maxInactiveFramesForLostPoints(240),
        m_maxInactiveFramesForActivePoints(480),
        m_pointSmoothingFactor(0.75),
        m_pointDeadBandSmoothingFactor(0.05),
        m_pointSmoothingDeadZone(50)      //mm
    {}

    PointProcessor::~PointProcessor()
    {}

    void PointProcessor::initialize_common_calculations(TrackingMatrices& matrices)
    {
        auto scalingMapper = get_scaling_mapper(matrices);

        segmentation::calculate_basic_score(matrices.depth,
                                            matrices.basicScore,
                                            m_heightScoreFactor,
                                            m_depthScoreFactor,
                                            scalingMapper);

        segmentation::calculate_segment_area(matrices.depth,
                                             matrices.area,
                                             matrices.areaSqrt,
                                             scalingMapper);

    }

    void PointProcessor::updateTrackedPoints(TrackingMatrices& matrices)
    {
        auto scalingMapper = get_scaling_mapper(matrices);

        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            //TODO take this and make it a method on TrackedPoint
            TrackedPoint& trackedPoint = *iter;
            updateTrackedPoint(matrices, scalingMapper, trackedPoint);
        }
    }

    void PointProcessor::updateTrackedPoint(TrackingMatrices& matrices,
                                            ScalingCoordinateMapper& scalingMapper,
                                            TrackedPoint& trackedPoint)
    {
        const float width = matrices.depth.cols;
        const float height = matrices.depth.rows;

        ++trackedPoint.inactiveFrameCount;

        cv::Point seedPosition = trackedPoint.position;
        float referenceDepth = trackedPoint.worldPosition.z;

        TrackingData updateTrackingData(matrices,
                                        seedPosition,
                                        referenceDepth,
                                        m_updateCycleBandwidthDepth,
                                        trackedPoint.pointType,
                                        m_iterationMaxTracking,
                                        m_maxSegmentationDist,
                                        VELOCITY_POLICY_IGNORE,
                                        m_edgeDistanceScoreFactor,
                                        m_targetEdgeDistance,
                                        m_pointInertiaFactor,
                                        m_pointInertiaRadius);

        cv::Point newTargetPoint = segmentation::converge_track_point_from_seed(updateTrackingData);

        validateAndUpdateTrackedPoint(matrices, scalingMapper, trackedPoint, newTargetPoint);

        //lost a tracked point, try to guess the position using previous position delta for second chance to recover

        if (trackedPoint.trackingStatus != TrackingStatus::Tracking && cv::norm(trackedPoint.worldDeltaPosition) > 0)
        {
            auto estimatedWorldPosition = trackedPoint.worldPosition + trackedPoint.worldDeltaPosition;

            cv::Point3f estimatedPosition = scalingMapper.convert_world_to_depth(estimatedWorldPosition);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(matrices,
                                             seedPosition,
                                             referenceDepth,
                                             m_createCycleBandwidthDepth,
                                             trackedPoint.pointType,
                                             m_iterationMaxTracking,
                                             m_maxSegmentationDist,
                                             VELOCITY_POLICY_IGNORE,
                                             m_edgeDistanceScoreFactor,
                                             m_targetEdgeDistance,
                                             m_pointInertiaFactor,
                                             m_pointInertiaRadius);

            newTargetPoint = segmentation::converge_track_point_from_seed(recoverTrackingData);

            validateAndUpdateTrackedPoint(matrices, scalingMapper, trackedPoint, newTargetPoint);

            if (trackedPoint.trackingStatus == TrackingStatus::Tracking)
            {
                //printf("Recovered point %d\n", trackedPoint.trackingId);
            }
        }
    }

    void PointProcessor::reset()
    {
        m_trackedPoints.clear();
        m_nextTrackingId = 0;
    }

    float PointProcessor::get_point_area(TrackingMatrices& matrices, const cv::Point& point)
    {
        auto scalingMapper = get_scaling_mapper(matrices);

        float area = segmentation::count_neighborhood_area(matrices.layerSegmentation,
                                                           matrices.depth,
                                                           matrices.area,
                                                           point,
                                                           m_areaBandwidth,
                                                           m_areaBandwidthDepth,
                                                           scalingMapper);

        return area;
    }

    bool PointProcessor::is_valid_point_area(TrackingMatrices& matrices, const cv::Point& targetPoint)
    {
        float area = get_point_area(matrices, targetPoint);

        bool validPointArea = area > m_minArea && area < m_maxArea;
        return validPointArea;
    }

    void PointProcessor::update_full_resolution_points(TrackingMatrices& matrices)
    {
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            //TODO take this and make it a method on TrackedPoint
            TrackedPoint& trackedPoint = *iter;

            float resizeFactor = get_resize_factor(matrices);

            //add 0.5 to center on the middle of the pixel
            trackedPoint.fullSizePosition.x = (trackedPoint.position.x + 0.5) * resizeFactor;
            trackedPoint.fullSizePosition.y = (trackedPoint.position.y + 0.5) * resizeFactor;

            if (trackedPoint.trackingStatus == TrackingStatus::Tracking &&
                trackedPoint.pointType == TrackedPointType::ActivePoint)
            {
                cv::Point refinedPosition = get_refined_high_res_position(matrices, trackedPoint);

                float refinedDepth = matrices.depthFullSize.at<float>(trackedPoint.fullSizePosition);

                cv::Point3f worldPosition = cv_convert_depth_to_world(matrices.fullSizeMapper,
                                                                      refinedPosition.x,
                                                                      refinedPosition.y,
                                                                      refinedDepth);

                cv::Point3f smoothedWorldPosition = smooth_world_positions(trackedPoint.fullSizeWorldPosition, worldPosition);

                update_tracked_point_from_world_position(trackedPoint,
                                                         smoothedWorldPosition,
                                                         resizeFactor,
                                                         matrices.fullSizeMapper);
            }
            else
            {
                trackedPoint.fullSizeWorldPosition = trackedPoint.worldPosition;
                trackedPoint.fullSizeWorldDeltaPosition = trackedPoint.worldDeltaPosition;
            }
        }
    }

    cv::Point PointProcessor::get_refined_high_res_position(TrackingMatrices& matrices, 
                                                            const TrackedPoint& trackedPoint)
    {
        assert(trackedPoint.pointType == TrackedPointType::ActivePoint);

        int fullWidth = matrices.depthFullSize.cols;
        int fullHeight = matrices.depthFullSize.rows;
        int processingWidth = matrices.depth.cols;
        int processingHeight = matrices.depth.rows;
        
        int fullSizeX = trackedPoint.fullSizePosition.x;
        int fullSizeY = trackedPoint.fullSizePosition.y;
        int windowLeft = MAX(0, MIN(fullWidth - processingWidth, fullSizeX - processingWidth / 2));
        int windowTop = MAX(0, MIN(fullHeight - processingHeight, fullSizeY - processingHeight / 2));

        //Create a window into the full size data
        cv::Mat roi = matrices.depthFullSize(cv::Rect(windowLeft, windowTop, processingWidth, processingHeight));
        //copyTo for now so .at works with local coords in functions that use it
        roi.copyTo(matrices.depth);

        //initialize_common_calculations(matrices);
        ScalingCoordinateMapper roiMapper(matrices.fullSizeMapper, 1.0, windowLeft, windowTop);

        segmentation::calculate_basic_score(matrices.depth,
                                            matrices.basicScore,
                                            m_heightScoreFactor,
                                            m_depthScoreFactor,
                                            roiMapper);

        segmentation::calculate_segment_area(matrices.depth,
                                             matrices.area,
                                             matrices.areaSqrt,
                                             roiMapper);

        cv::Point roiPosition(fullSizeX - windowLeft, fullSizeY - windowTop);

        float referenceDepth = trackedPoint.worldPosition.z;

        TrackingData refinementTrackingData(matrices,
                                            roiPosition,
                                            referenceDepth,
                                            m_updateCycleBandwidthDepth,
                                            TrackedPointType::ActivePoint,
                                            m_iterationMaxRefinement,
                                            m_maxSegmentationDist,
                                            VELOCITY_POLICY_IGNORE,
                                            m_edgeDistanceScoreFactor,
                                            m_targetEdgeDistance,
                                            m_pointInertiaFactor,
                                            m_pointInertiaRadius);

        cv::Point targetPoint = segmentation::converge_track_point_from_seed(refinementTrackingData);

        int refinedFullSizeX = targetPoint.x + windowLeft;
        int refinedFullSizeY = targetPoint.y + windowTop;
        
        return cv::Point(refinedFullSizeX, refinedFullSizeY);
    }

    cv::Point3f PointProcessor::smooth_world_positions(const cv::Point3f& oldWorldPosition, 
                                                       const cv::Point3f& newWorldPosition)
    {
        float smoothingFactor = m_pointSmoothingFactor;

        float delta = cv::norm(newWorldPosition - oldWorldPosition);
        if (delta < m_pointSmoothingDeadZone)
        {
            float factorRamp = delta / m_pointSmoothingDeadZone;
            smoothingFactor = m_pointSmoothingFactor * factorRamp + m_pointDeadBandSmoothingFactor * (1 - factorRamp);
        }

        return oldWorldPosition * (1 - smoothingFactor) + newWorldPosition * smoothingFactor;
    }

    void PointProcessor::update_tracked_point_from_world_position(TrackedPoint& trackedPoint, 
                                                                  const cv::Point3f& newWorldPosition, 
                                                                  const float resizeFactor,
                                                                  const CoordinateMapper& fullSizeMapper)
    {
        cv::Point3f fullSizeDepthPosition = cv_convert_world_to_depth(fullSizeMapper, newWorldPosition);

        trackedPoint.fullSizePosition = cv::Point(fullSizeDepthPosition.x, fullSizeDepthPosition.y);

        trackedPoint.position.x = fullSizeDepthPosition.x / resizeFactor;
        trackedPoint.position.y = fullSizeDepthPosition.y / resizeFactor;

        cv::Point3f deltaPosition = newWorldPosition - trackedPoint.fullSizeWorldPosition;
        trackedPoint.fullSizeWorldPosition = newWorldPosition;
        trackedPoint.fullSizeWorldDeltaPosition = deltaPosition;
    }

    void PointProcessor::validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                                       ScalingCoordinateMapper& scalingMapper,
                                                       TrackedPoint& trackedPoint,
                                                       const cv::Point& newTargetPoint)
    {
        bool updatedPoint = false;
        
        if (newTargetPoint.x != -1 && newTargetPoint.y != -1)
        {
            float depth = matrices.depth.at<float>(newTargetPoint);

            cv::Point3f worldPosition = scalingMapper.convert_depth_to_world(newTargetPoint.x, newTargetPoint.y, depth);

            auto dist = cv::norm(worldPosition - trackedPoint.worldPosition);
            auto steadyDist = cv::norm(worldPosition - trackedPoint.steadyWorldPosition);

            bool validPointArea = is_valid_point_area(matrices, newTargetPoint);

            if (dist < m_maxJumpDist && validPointArea)
            {
                updatedPoint = true;

                cv::Point3f deltaPosition = worldPosition - trackedPoint.worldPosition;
                trackedPoint.worldPosition = worldPosition;
                trackedPoint.worldDeltaPosition = deltaPosition;

                trackedPoint.position = newTargetPoint;
                if (steadyDist > m_steadyDeadBandRadius)
                {
                    trackedPoint.steadyWorldPosition = worldPosition;
                    trackedPoint.inactiveFrameCount = 0;
                }

                if (trackedPoint.inactiveFrameCount < m_maxInactiveFramesToBeConsideredActive)
                {
                    trackedPoint.activeFrameCount++;
                    if (trackedPoint.activeFrameCount > m_minActiveFramesToLockTracking)
                    {
                        trackedPoint.pointType = TrackedPointType::ActivePoint;
                    }
                }
            }
        }

        assert(trackedPoint.trackingStatus != TrackingStatus::Dead);

        if (updatedPoint)
        {
            trackedPoint.trackingStatus = TrackingStatus::Tracking;
        }
        else
        {
            trackedPoint.trackingStatus = TrackingStatus::Lost;
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
                bool bothNotDead = tracked.trackingStatus != TrackingStatus::Dead && otherTracked.trackingStatus != TrackingStatus::Dead;
                if (tracked.trackingId != otherTracked.trackingId && bothNotDead && tracked.position == otherTracked.position)
                {
                    tracked.activeFrameCount = MAX(tracked.activeFrameCount, otherTracked.activeFrameCount);
                    tracked.inactiveFrameCount = MIN(tracked.inactiveFrameCount, otherTracked.inactiveFrameCount);
                    if (otherTracked.pointType == TrackedPointType::ActivePoint && tracked.pointType != TrackedPointType::ActivePoint)
                    {
                        tracked.trackingId = otherTracked.trackingId;
                        tracked.pointType = TrackedPointType::ActivePoint;
                    }
                    otherTracked.trackingStatus = TrackingStatus::Dead;
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
            if (tracked.pointType == TrackedPointType::ActivePoint)
            {
                if (tracked.trackingStatus == TrackingStatus::Lost)
                {
                    max = m_maxInactiveFramesForLostPoints;
                }
                else
                {
                    max = m_maxInactiveFramesForActivePoints;
                }
            }
            //if inactive for more than a certain number of frames, or dead, remove point
            if (tracked.inactiveFrameCount > max || tracked.trackingStatus == TrackingStatus::Dead)
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
                                  m_createCycleBandwidthDepth,
                                  TrackedPointType::CandidatePoint,
                                  m_iterationMaxInitial,
                                  m_maxSegmentationDist,
                                  VELOCITY_POLICY_RESET_TTL,
                                  m_edgeDistanceScoreFactor,
                                  m_targetEdgeDistance,
                                  m_pointInertiaFactor,
                                  m_pointInertiaRadius);

        cv::Point targetPoint = segmentation::converge_track_point_from_seed(trackingData);

        bool validPointArea = is_valid_point_area(matrices, targetPoint);

        if (targetPoint.x != -1 && targetPoint.y != -1 && validPointArea)
        {
            bool existingPoint = false;
            
            float depth = matrices.depth.at<float>(targetPoint);

            auto scalingMapper = get_scaling_mapper(matrices);
            cv::Point3f worldPosition = scalingMapper.convert_depth_to_world(targetPoint.x,
                                                                                   targetPoint.y, 
                                                                                   depth);

            for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
            {
                TrackedPoint& tracked = *iter;
                if (tracked.trackingStatus != TrackingStatus::Dead)
                {
                    float dist = cv::norm(tracked.worldPosition - worldPosition);
                    float maxDist = m_maxMatchDistDefault;
                    bool activeLost = tracked.pointType == TrackedPointType::ActivePoint && tracked.trackingStatus == TrackingStatus::Lost;
                    if (activeLost)
                    {
                        maxDist = m_maxMatchDistLostActive;
                    }
                    if (dist < maxDist)
                    {
                        tracked.inactiveFrameCount = 0;
                        if (activeLost)
                        {
                            //Recover a lost point -- move it to the recovery position
                            tracked.position = targetPoint;
                            tracked.worldPosition = worldPosition;
                            tracked.worldDeltaPosition = cv::Point3f();
                        }
                        tracked.trackingStatus = TrackingStatus::Tracking;
                        existingPoint = true;
                        break;
                    }
                }
            }
            if (!existingPoint)
            {
                TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId);
                newPoint.pointType = TrackedPointType::CandidatePoint;
                newPoint.trackingStatus = TrackingStatus::Tracking;
                ++m_nextTrackingId;
                m_trackedPoints.push_back(newPoint);
            }
        }
    }
}}}
