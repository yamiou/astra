#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "Segmentation.h"

namespace sensekit { namespace plugins { namespace hand {

    PointProcessor::PointProcessor(PluginLogger& pluginLogger, HandSettings& settings) :
        m_settings(settings),
        m_logger(pluginLogger),
        m_segmentationBandwidthDepthNear(settings.segmentationBandwidthDepthNear), //mm
        m_segmentationBandwidthDepthFar(settings.segmentationBandwidthDepthFar),  //mm
        m_maxMatchDistLostActive(settings.maxMatchDistLostActive),  //mm
        m_maxMatchDistDefault(settings.maxMatchDistDefault),     //mm
        m_iterationMaxInitial(settings.iterationMaxInitial),
        m_iterationMaxTracking(settings.iterationMaxTracking),
        m_iterationMaxRefinement(settings.iterationMaxRefinement),
        m_minArea(settings.minArea),                   //mm^2
        m_maxArea(settings.maxArea),               //mm^2
        m_areaBandwidth(settings.areaBandwidth),           //mm
        m_areaBandwidthDepth(settings.areaBandwidthDepth),      //mm
        m_maxSegmentationDist(settings.maxSegmentationDist),     //mm
        m_steadyDeadBandRadius(settings.steadyDeadBandRadius),    //mm
        m_targetEdgeDistance(settings.targetEdgeDistance),       //mm
        m_heightScoreFactor(settings.heightScoreFactor),
        m_depthScoreFactor(settings.depthScoreFactor),
        m_edgeDistanceScoreFactor(settings.edgeDistanceScoreFactor),
        m_pointInertiaFactor(settings.pointInertiaFactor),
        m_pointInertiaRadius(settings.pointInertiaRadius),       //mm
        m_maxInactiveFramesForCandidatePoints(settings.maxInactiveFramesForCandidatePoints),
        m_maxInactiveFramesForLostPoints(settings.maxInactiveFramesForLostPoints),
        m_maxInactiveFramesForActivePoints(settings.maxInactiveFramesForActivePoints),
        m_pointSmoothingFactor(settings.pointSmoothingFactor),
        m_pointDeadBandSmoothingFactor(settings.pointDeadBandSmoothingFactor),
        m_pointSmoothingDeadZone(settings.pointSmoothingDeadZone),     //mm
        m_foregroundRadius1(settings.foregroundRadius1),
        m_foregroundRadius2(settings.foregroundRadius2),
        m_foregroundRadiusMaxPercent1(settings.foregroundRadiusMaxPercent1),
        m_foregroundRadiusMaxPercent2(settings.foregroundRadiusMaxPercent2),
        m_maxFailedTestsInProbation(settings.maxFailedTestsInProbation),
        m_probationFrameCount(settings.probationFrameCount),
        m_maxFailedTestsInProbationActivePoints(settings.maxFailedTestsInProbationActivePoints),
        m_secondChanceMinDistance(settings.secondChanceMinDistance)
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
                                        trackedPoint.referenceAreaSqrt,
                                        m_segmentationBandwidthDepthNear,
                                        m_segmentationBandwidthDepthFar,
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
        
        auto xyDelta = trackedPoint.worldDeltaPosition;
        xyDelta.z = 0;
        double xyDeltaNorm = cv::norm(xyDelta);
        if (trackedPoint.trackingStatus != TrackingStatus::Tracking && 
            newTargetPoint == segmentation::INVALID_POINT && 
            xyDeltaNorm > m_secondChanceMinDistance)
        {
            auto movementDirection = xyDelta * (1.0f / xyDeltaNorm);
            auto estimatedWorldPosition = trackedPoint.worldPosition + movementDirection * m_maxSegmentationDist;

            cv::Point3f estimatedPosition = scalingMapper.convert_world_to_depth(estimatedWorldPosition);

            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));
            referenceDepth = estimatedPosition.z;

            TrackingData recoverTrackingData(matrices,
                                             seedPosition,
                                             referenceDepth,
                                             trackedPoint.referenceAreaSqrt,
                                             m_segmentationBandwidthDepthNear,
                                             m_segmentationBandwidthDepthFar,
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
                    m_logger.trace("updateTrackedPoint 2nd chance recovered #%d",
                                  trackedPoint.trackingId);
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

    bool PointProcessor::test_point_in_range(TrackingMatrices& matrices,
                                             const cv::Point& targetPoint,
                                             TrackingStatus status,
                                             int trackingId)
    {
        if (targetPoint == segmentation::INVALID_POINT ||
            targetPoint.x < 0 || targetPoint.x >= matrices.depth.cols ||
            targetPoint.y < 0 || targetPoint.y >= matrices.depth.rows)
        {
            if (status == TrackingStatus::Tracking)
            {
                m_logger.trace("test_point_in_range failed #%d: position: (%d, %d)",
                               trackingId,
                               targetPoint.x,
                               targetPoint.y);
            }
            return false;
        }

        return true;
    }

    bool PointProcessor::test_point_area(TrackingMatrices& matrices, 
                                         const cv::Point& targetPoint, 
                                         TrackingStatus status,
                                         int trackingId)
    {
        float area = get_point_area(matrices, targetPoint);

        bool validPointArea = area > m_minArea && area < m_maxArea;

        if (status == TrackingStatus::Tracking && !validPointArea)
        {
            m_logger.trace("test_point_area failed #%d: area %f not within [%f, %f]",
                           trackingId,
                           area,
                           m_minArea,
                           m_maxArea);
        }
        /*else if (status == TrackingStatus::NotTracking && validPointArea)
        {
            m_logger.trace("test_point_area passed: area %f within [%f, %f]",
                           area,
                           m_minArea,
                           m_maxArea);
        }*/

        return validPointArea;
    }

    bool PointProcessor::test_foreground_radius_percentage(TrackingMatrices& matrices, 
                                                           const cv::Point& targetPoint, 
                                                           TrackingStatus status,
                                                           int trackingId)
    {
        auto scalingMapper = get_scaling_mapper(matrices);

        float percentForeground1 = segmentation::get_percent_foreground_along_circumference(matrices.depth,
                                                                                            matrices.layerSegmentation,
                                                                                            targetPoint,
                                                                                            m_foregroundRadius1,
                                                                                            scalingMapper);

        float percentForeground2 = segmentation::get_percent_foreground_along_circumference(matrices.depth,
                                                                                            matrices.layerSegmentation,
                                                                                            targetPoint,
                                                                                            m_foregroundRadius2,
                                                                                            scalingMapper);

        bool passTest1 = percentForeground1 < m_foregroundRadiusMaxPercent1;
        bool passTest2 = percentForeground2 < m_foregroundRadiusMaxPercent2;
        bool passed = passTest1 && passTest2;

        if (status == TrackingStatus::Tracking && !passed)
        {
            m_logger.trace("test_foreground_radius_percentage failed #%d: perc1 %f (max %f) perc2 %f (max %f)",
                           trackingId,
                           percentForeground1,
                           m_foregroundRadiusMaxPercent1,
                           percentForeground2,
                           m_foregroundRadiusMaxPercent2);
        }
        /*else if (status == TrackingStatus::NotTracking && passed)
        {
            m_logger.trace("test_foreground_radius_percentage passed: perc1 %f (max %f) perc2 %f (max %f)",
                           percentForeground1,
                           m_foregroundRadiusMaxPercent1,
                           percentForeground2,
                           m_foregroundRadiusMaxPercent2);
        }*/
        return passed;
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
                cv::Point3f refinedWorldPosition = get_refined_high_res_position(matrices, trackedPoint);

                cv::Point3f smoothedWorldPosition = smooth_world_positions(trackedPoint.fullSizeWorldPosition, refinedWorldPosition);
                
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

    void PointProcessor::update_trajectories()
    {
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            //TODO take this and make it a method on TrackedPoint
            TrackedPoint& trackedPoint = *iter;
            int trackingId = trackedPoint.trackingId;
            auto it = m_trajectories.find(trackingId);

            if (it == m_trajectories.end())
            {
                TrajectoryAnalyzer analyzer(trackingId, m_logger, m_settings);
                analyzer.update(trackedPoint);
                m_trajectories.insert(std::make_pair(trackingId, analyzer));
            }
            else
            {
                TrajectoryAnalyzer& analyzer = it->second;
                analyzer.update(trackedPoint);

                if (analyzer.isWaveGesture())
                {
                    end_probation(trackedPoint);
                    trackedPoint.pointType = TrackedPointType::ActivePoint;
                }
            }
        }
    }

    cv::Point3f PointProcessor::get_refined_high_res_position(TrackingMatrices& matrices, 
                                                            const TrackedPoint& trackedPoint)
    {
        assert(trackedPoint.pointType == TrackedPointType::ActivePoint);

        float referenceDepth = trackedPoint.worldPosition.z;
        if (referenceDepth == 0)
        {
            return trackedPoint.worldPosition;
        }

        int fullWidth = matrices.depthFullSize.cols;
        int fullHeight = matrices.depthFullSize.rows;
        int processingWidth = matrices.depth.cols;
        int processingHeight = matrices.depth.rows;
        
        int fullSizeX = trackedPoint.fullSizePosition.x;
        int fullSizeY = trackedPoint.fullSizePosition.y;
        int windowLeft = MAX(0, MIN(fullWidth - processingWidth, fullSizeX - processingWidth / 2));
        int windowTop = MAX(0, MIN(fullHeight - processingHeight, fullSizeY - processingHeight / 2));

        cv::Point roiPosition(fullSizeX - windowLeft, fullSizeY - windowTop);

        float referenceAreaSqrt = matrices.areaSqrt.at<float>(roiPosition);
        if (referenceAreaSqrt == 0)
        {
            return trackedPoint.worldPosition;
        }

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

        TrackingData refinementTrackingData(matrices,
                                            roiPosition,
                                            referenceDepth,
                                            referenceAreaSqrt,
                                            m_segmentationBandwidthDepthNear,
                                            m_segmentationBandwidthDepthFar,
                                            TrackedPointType::ActivePoint,
                                            m_iterationMaxRefinement,
                                            m_maxSegmentationDist,
                                            VELOCITY_POLICY_IGNORE,
                                            m_edgeDistanceScoreFactor,
                                            m_targetEdgeDistance,
                                            m_pointInertiaFactor,
                                            m_pointInertiaRadius);

        cv::Point targetPoint = segmentation::converge_track_point_from_seed(refinementTrackingData);

        if (targetPoint == segmentation::INVALID_POINT)
        {
            return trackedPoint.worldPosition;
        }

        int refinedFullSizeX = targetPoint.x + windowLeft;
        int refinedFullSizeY = targetPoint.y + windowTop;
        
        float refinedDepth = matrices.depthFullSize.at<float>(refinedFullSizeY, refinedFullSizeX);

        if (refinedDepth == 0)
        {
            refinedDepth = trackedPoint.worldPosition.z;
        }

        cv::Point3f refinedWorldPosition = cv_convert_depth_to_world(matrices.fullSizeMapper,
                                                                     refinedFullSizeX,
                                                                     refinedFullSizeY,
                                                                     refinedDepth);

        return refinedWorldPosition;
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

        //trackedPoint.position.x = fullSizeDepthPosition.x / resizeFactor;
        //trackedPoint.position.y = fullSizeDepthPosition.y / resizeFactor;

        cv::Point3f deltaPosition = newWorldPosition - trackedPoint.fullSizeWorldPosition;
        trackedPoint.fullSizeWorldPosition = newWorldPosition;
        trackedPoint.fullSizeWorldDeltaPosition = deltaPosition;
    }

    void PointProcessor::start_probation(TrackedPoint& trackedPoint)
    {
        if (!trackedPoint.isInProbation)
        {
            trackedPoint.isInProbation = true;
            trackedPoint.probationFrameCount = 0;
            trackedPoint.failedTestCount = 0;
            trackedPoint.failedInRangeTestCount = 0;
        }
    }

    void PointProcessor::end_probation(TrackedPoint& trackedPoint)
    {
        trackedPoint.isInProbation = false;
        trackedPoint.failedTestCount = 0;
        trackedPoint.failedInRangeTestCount = 0;
    }

    void PointProcessor::update_tracked_point_data(TrackingMatrices& matrices, ScalingCoordinateMapper& scalingMapper, TrackedPoint& trackedPoint, const cv::Point& newTargetPoint)
    {
        float depth = matrices.depth.at<float>(newTargetPoint);
        cv::Point3f worldPosition = scalingMapper.convert_depth_to_world(newTargetPoint.x, newTargetPoint.y, depth);

        cv::Point3f deltaPosition = worldPosition - trackedPoint.worldPosition;
        trackedPoint.worldPosition = worldPosition;
        trackedPoint.worldDeltaPosition = deltaPosition;

        trackedPoint.position = newTargetPoint;
        trackedPoint.referenceAreaSqrt = matrices.areaSqrt.at<float>(trackedPoint.position);

        auto steadyDist = cv::norm(worldPosition - trackedPoint.steadyWorldPosition);

        if (steadyDist > m_steadyDeadBandRadius)
        {
            trackedPoint.steadyWorldPosition = worldPosition;
            trackedPoint.inactiveFrameCount = 0;
        }
    }

    void PointProcessor::validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                                       ScalingCoordinateMapper& scalingMapper,
                                                       TrackedPoint& trackedPoint,
                                                       const cv::Point& newTargetPoint)
    {
        if(trackedPoint.trackingStatus == TrackingStatus::Dead)
        {
            return;
        }

        auto oldStatus = trackedPoint.trackingStatus;

        bool validPointInRange = test_point_in_range(matrices, newTargetPoint, trackedPoint.trackingStatus, trackedPoint.trackingId);
        bool validPointArea = false;
        bool validRadiusTest = false;
        
        if (validPointInRange)
        {
            validPointArea = test_point_area(matrices, newTargetPoint, trackedPoint.trackingStatus, trackedPoint.trackingId);
            validRadiusTest = test_foreground_radius_percentage(matrices, newTargetPoint, trackedPoint.trackingStatus, trackedPoint.trackingId);
        }

        bool passAllTests = validPointInRange && validPointArea && validRadiusTest;
        
        if (passAllTests)
        {
            trackedPoint.trackingStatus = TrackingStatus::Tracking;
            if (trackedPoint.pointType == TrackedPointType::ActivePoint)
            {
                trackedPoint.failedTestCount = 0;
                trackedPoint.failedInRangeTestCount = 0;
            }
        }
        else
        {
            start_probation(trackedPoint);
            if (!passAllTests)
            {
                ++trackedPoint.failedTestCount;
            }
            if (!validPointInRange)
            {
                ++trackedPoint.failedInRangeTestCount;
            }
        }

        if (trackedPoint.isInProbation)
        {
            bool exitProbation = false;
            if (trackedPoint.pointType == TrackedPointType::ActivePoint)
            {
                if (trackedPoint.failedInRangeTestCount >= m_maxFailedTestsInProbationActivePoints)
                {
                    //failed because of out of range points, perhaps certain artifacts like finger pointed at camera
                    //go to Lost status for a short time

                    //failed N consecutive tests within the probation period
                    //gave the active point a few extra frames to recover
                    trackedPoint.trackingStatus = TrackingStatus::Lost;
                    exitProbation = true;
                }
                else if (trackedPoint.failedTestCount >= m_maxFailedTestsInProbationActivePoints)
                {
                    //had valid in range points but must have failed the real tests

                    //failed N consecutive tests within the probation period
                    //gave the active point a few extra frames to recover
                    trackedPoint.trackingStatus = TrackingStatus::Dead;
                    exitProbation = true;
                }
            }
            else if (trackedPoint.failedTestCount >= m_maxFailedTestsInProbation)
            {
                //failed N tests total (non-consecutive) within the probation period
                //too many failed tests, so long...
                trackedPoint.trackingStatus = TrackingStatus::Dead;
                exitProbation = true;
            }

            ++trackedPoint.probationFrameCount;
            if (trackedPoint.probationFrameCount > m_probationFrameCount || exitProbation)
            {
                //you're out of probation, but we're keeping an eye on you...
                end_probation(trackedPoint);
            }
        }

        if (passAllTests)
        {
            update_tracked_point_data(matrices, scalingMapper, trackedPoint, newTargetPoint);
        }

        if (trackedPoint.trackingStatus != oldStatus)
        {
            m_logger.trace("validateAndUpdateTrackedPoint: #%d status %s --> status %s", 
                           trackedPoint.trackingId, 
                           tracking_status_to_string(oldStatus).c_str(),
                           tracking_status_to_string(trackedPoint.trackingStatus).c_str());
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
                m_trajectories.erase(tracked.trackingId);

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
        float referenceAreaSqrt = matrices.areaSqrt.at<float>(seedPosition);
        if (referenceDepth == 0 || referenceAreaSqrt == 0 || seedPosition == segmentation::INVALID_POINT)
        {
            //Cannot expect to properly segment when the seedPosition has zero depth
            return;
        }

        TrackingData createTrackingData(matrices,
                                        seedPosition,
                                        referenceDepth,
                                        referenceAreaSqrt,
                                        m_segmentationBandwidthDepthNear,
                                        m_segmentationBandwidthDepthFar,
                                        TrackedPointType::CandidatePoint,
                                        m_iterationMaxInitial,
                                        m_maxSegmentationDist,
                                        VELOCITY_POLICY_RESET_TTL,
                                        m_edgeDistanceScoreFactor,
                                        m_targetEdgeDistance,
                                        m_pointInertiaFactor,
                                        m_pointInertiaRadius);

        cv::Point targetPoint = segmentation::converge_track_point_from_seed(createTrackingData);
        
        bool validPointInRange = test_point_in_range(matrices, targetPoint, TrackingStatus::NotTracking, -1);

        if (!validPointInRange)
        {
            return;
        }

        bool validPointArea = test_point_area(matrices, targetPoint, TrackingStatus::NotTracking, -1);
        bool validRadiusTest = test_foreground_radius_percentage(matrices, targetPoint, TrackingStatus::NotTracking, -1);

        if (!validPointArea || !validRadiusTest)
        {
            return;
        }

        bool existingPoint = false;
            
        float depth = matrices.depth.at<float>(targetPoint);

        auto scalingMapper = get_scaling_mapper(matrices);
        cv::Point3f worldPosition = scalingMapper.convert_depth_to_world(targetPoint.x,
                                                                         targetPoint.y, 
                                                                         depth);

        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            TrackedPoint& trackedPoint = *iter;
            if (trackedPoint.trackingStatus != TrackingStatus::Dead)
            {
                float dist = cv::norm(trackedPoint.worldPosition - worldPosition);
                float maxDist = m_maxMatchDistDefault;
                bool lostPoint = trackedPoint.trackingStatus == TrackingStatus::Lost;
                if (lostPoint && trackedPoint.pointType == TrackedPointType::ActivePoint)
                {
                    maxDist = m_maxMatchDistLostActive;
                }
                if (dist < maxDist)
                {
                    trackedPoint.inactiveFrameCount = 0;
                    if (lostPoint)
                    {
                        //Recover a lost point -- move it to the recovery position
                        trackedPoint.position = targetPoint;
                        trackedPoint.referenceAreaSqrt = matrices.areaSqrt.at<float>(trackedPoint.position);

                        trackedPoint.worldPosition = worldPosition;
                        trackedPoint.worldDeltaPosition = cv::Point3f();

                        m_logger.trace("createCycle: Recovered #%d",
                                        trackedPoint.trackingId);
                        
                        //it could be faulty recovery, so start out in probation just like a new point
                        start_probation(trackedPoint);
                    }
                    trackedPoint.trackingStatus = TrackingStatus::Tracking;
                    existingPoint = true;
                    break;
                }
            }
        }
        if (!existingPoint)
        {
            m_logger.trace("createCycle: Created new point #%d",
                           m_nextTrackingId);

            TrackedPoint newPoint(targetPoint, worldPosition, m_nextTrackingId);
            newPoint.pointType = TrackedPointType::CandidatePoint;
            newPoint.trackingStatus = TrackingStatus::Tracking;
            ++m_nextTrackingId;
            m_trackedPoints.push_back(newPoint);
            start_probation(newPoint);
        }
    }
}}}
