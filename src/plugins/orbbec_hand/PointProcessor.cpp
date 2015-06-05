#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "Segmentation.h"
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace hand {

    PointProcessor::PointProcessor(PluginLogger& pluginLogger, PointProcessorSettings& settings) :
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
    {
        PROFILE_FUNC();
    }

    PointProcessor::~PointProcessor()
    {
        PROFILE_FUNC();
    }

    void PointProcessor::calculate_area(TrackingMatrices& matrices, ScalingCoordinateMapper mapper)
    {
        PROFILE_FUNC();

        const sensekit::Vector3f* fullSizeWorldPoints = matrices.fullSizeWorldPoints;
        sensekit::Vector3f* worldPoints = matrices.worldPoints;
        auto depthToWorldData = matrices.depthToWorldData;
        cv::Mat& areaMatrix = matrices.area;
        cv::Mat& areaSqrtMatrix = matrices.areaSqrt;

        cv::Size depthSize = matrices.depth.size();

        areaMatrix = cv::Mat::zeros(depthSize, CV_32FC1);
        areaSqrtMatrix = cv::Mat::zeros(depthSize, CV_32FC1);

        int fullSizeWidth = matrices.depthFullSize.cols;
        int width = depthSize.width;
        int height = depthSize.height;

        float offsetX = mapper.offsetX();
        float offsetY = mapper.offsetY();
        float scale = mapper.scale();
        int intScale = static_cast<int>(scale);

        for (int y = 0; y < height; ++y)
        {
            float* areaRow = areaMatrix.ptr<float>(y);
            float* areaSqrtRow = areaSqrtMatrix.ptr<float>(y);

            for (int x = 0; x < width; ++x, ++worldPoints, ++areaRow, ++areaSqrtRow)
            {
                int fullSizeIndex = (x + y * fullSizeWidth) * intScale;
                const Vector3f& p = fullSizeWorldPoints[fullSizeIndex];
                *worldPoints = p;
                const float depth = p.z;

                if (depth != 0)
                {
                    float depthX = (x + 1 + offsetX) * scale;
                    float depthY = (y + 1 + offsetY) * scale;
                    float normalizedX = depthX / depthToWorldData.resolutionX - .5f;
                    float normalizedY = .5f - depthY / depthToWorldData.resolutionY;

                    float wx = normalizedX * depth * depthToWorldData.xzFactor;
                    float wy = normalizedY * depth * depthToWorldData.yzFactor;

                    float deltaX = wx - p.x;
                    float deltaY = wy - p.y;

                    float area = fabs(deltaX * deltaY);

                    *areaRow = area;
                    *areaSqrtRow = sqrt(area);
                }
                else
                {
                    *areaRow = 0;
                    *areaSqrtRow = 0;
                }
            }
        }
    }

    void PointProcessor::initialize_common_calculations(TrackingMatrices& matrices)
    {
        PROFILE_FUNC();
        auto scalingMapper = get_scaling_mapper(matrices);

        calculate_area(matrices, scalingMapper);

        segmentation::calculate_basic_score(matrices.worldPoints,
                                            matrices.depth.size(),
                                            matrices.basicScore,
                                            m_heightScoreFactor,
                                            m_depthScoreFactor);

    }

    void PointProcessor::updateTrackedPoints(TrackingMatrices& matrices)
    {
        PROFILE_FUNC();
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
        PROFILE_FUNC();
        const float width = matrices.depth.cols;
        const float height = matrices.depth.rows;

        ++trackedPoint.inactiveFrameCount;

        TrackingData updateTrackingData(matrices,
                                        trackedPoint.position,
                                        trackedPoint.worldPosition,
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

            cv::Point seedPosition;
            seedPosition.x = MAX(0, MIN(width - 1, static_cast<int>(estimatedPosition.x)));
            seedPosition.y = MAX(0, MIN(height - 1, static_cast<int>(estimatedPosition.y)));

            TrackingData recoverTrackingData(matrices,
                                             seedPosition,
                                             estimatedWorldPosition,
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
        PROFILE_FUNC();
        m_trackedPoints.clear();
        m_nextTrackingId = 0;
    }

    float PointProcessor::get_point_area(TrackingMatrices& matrices, const cv::Point& point)
    {
        PROFILE_FUNC();
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
                                             int trackingId,
                                             bool outputLog)
    {
        PROFILE_FUNC();
        if (targetPoint == segmentation::INVALID_POINT ||
            targetPoint.x < 0 || targetPoint.x >= matrices.depth.cols ||
            targetPoint.y < 0 || targetPoint.y >= matrices.depth.rows)
        {
            if (outputLog)
            {
                m_logger.info("test_point_in_range failed #%d: position: (%d, %d)",
                              trackingId,
                              targetPoint.x,
                              targetPoint.y);
            }
            return false;
        }

        if (outputLog)
        {
            m_logger.info("test_point_in_range success #%d: position: (%d, %d)",
                          trackingId,
                          targetPoint.x,
                          targetPoint.y);
        }

        return true;
    }

    bool PointProcessor::test_point_area(TrackingMatrices& matrices,
                                         const cv::Point& targetPoint,
                                         int trackingId,
                                         bool outputLog)
    {
        PROFILE_FUNC();
        float area = get_point_area(matrices, targetPoint);

        bool validPointArea = area > m_minArea && area < m_maxArea;

        if (outputLog)
        {
            if (validPointArea)
            {
                m_logger.info("test_point_area passed #%d: area %f within [%f, %f]",
                              trackingId,
                              area,
                              m_minArea,
                              m_maxArea);
            }
            else
            {
                m_logger.info("test_point_area failed #%d: area %f not within [%f, %f]",
                              trackingId,
                              area,
                              m_minArea,
                              m_maxArea);
            }
        }

        return validPointArea;
    }

    bool PointProcessor::test_foreground_radius_percentage(TrackingMatrices& matrices,
                                                           const cv::Point& targetPoint,
                                                           int trackingId,
                                                           bool outputLog)
    {
        PROFILE_FUNC();
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

        if (outputLog)
        {
            if (passed)
            {
                m_logger.info("test_foreground_radius_percentage passed #%d: perc1 %f (max %f) perc2 %f (max %f)",
                              trackingId,
                              percentForeground1,
                              m_foregroundRadiusMaxPercent1,
                              percentForeground2,
                              m_foregroundRadiusMaxPercent2);
            }
            else
            {
                m_logger.info("test_foreground_radius_percentage failed #%d: perc1 %f (max %f) perc2 %f (max %f)",
                              trackingId,
                              percentForeground1,
                              m_foregroundRadiusMaxPercent1,
                              percentForeground2,
                              m_foregroundRadiusMaxPercent2);
            }
        }
        return passed;
    }

    void PointProcessor::update_full_resolution_points(TrackingMatrices& matrices)
    {
        PROFILE_FUNC();
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            TrackedPoint& trackedPoint = *iter;

            float resizeFactor = get_resize_factor(matrices);

            //add 0.5 to center on the middle of the pixel
            trackedPoint.fullSizePosition.x = (trackedPoint.position.x + 0.5) * resizeFactor;
            trackedPoint.fullSizePosition.y = (trackedPoint.position.y + 0.5) * resizeFactor;

            bool resizeNeeded = matrices.depthFullSize.cols != matrices.depth.cols;

            bool processRefinedPosition = false;

            if (resizeNeeded &&
                trackedPoint.trackingStatus == TrackingStatus::Tracking &&
                trackedPoint.pointType == TrackedPointType::ActivePoint)
            {
                cv::Point3f refinedWorldPosition = trackedPoint.worldPosition;
                if (processRefinedPosition)
                {
                    refinedWorldPosition = get_refined_high_res_position(matrices, trackedPoint);
                }

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
        PROFILE_FUNC();
        for (auto iter = m_trackedPoints.begin(); iter != m_trackedPoints.end(); ++iter)
        {
            //TODO take this and make it a method on TrackedPoint
            TrackedPoint& trackedPoint = *iter;
            int trackingId = trackedPoint.trackingId;
            auto it = m_trajectories.find(trackingId);

            if (it == m_trajectories.end())
            {
                TrajectoryAnalyzer analyzer(trackingId, m_logger, m_settings.trajectoryAnalyzerSettings);
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
        PROFILE_FUNC();
        assert(trackedPoint.pointType == TrackedPointType::ActivePoint);

        if (trackedPoint.worldPosition.z == 0)
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

        calculate_area(matrices, roiMapper);

        segmentation::calculate_basic_score(matrices.worldPoints,
                                            matrices.depth.size(),
                                            matrices.basicScore,
                                            m_heightScoreFactor,
                                            m_depthScoreFactor);

        TrackingData refinementTrackingData(matrices,
                                            roiPosition,
                                            trackedPoint.worldPosition,
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
        PROFILE_FUNC();
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
        PROFILE_FUNC();
        cv::Point3f fullSizeDepthPosition = cv_convert_world_to_depth(fullSizeMapper, newWorldPosition);

        trackedPoint.fullSizePosition = cv::Point(fullSizeDepthPosition.x, fullSizeDepthPosition.y);

        cv::Point3f deltaPosition = newWorldPosition - trackedPoint.fullSizeWorldPosition;
        trackedPoint.fullSizeWorldPosition = newWorldPosition;
        trackedPoint.fullSizeWorldDeltaPosition = deltaPosition;
    }

    void PointProcessor::start_probation(TrackedPoint& trackedPoint)
    {
        PROFILE_FUNC();
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
        PROFILE_FUNC();
        trackedPoint.isInProbation = false;
        trackedPoint.failedTestCount = 0;
        trackedPoint.failedInRangeTestCount = 0;
    }

    void PointProcessor::update_tracked_point_data(TrackingMatrices& matrices, ScalingCoordinateMapper& scalingMapper, TrackedPoint& trackedPoint, const cv::Point& newTargetPoint)
    {
        PROFILE_FUNC();
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
        PROFILE_FUNC();
        if(trackedPoint.trackingStatus == TrackingStatus::Dead)
        {
            return;
        }

        auto oldStatus = trackedPoint.trackingStatus;
        bool outputTestLog = false;

        bool validPointInRange = test_point_in_range(matrices, newTargetPoint, trackedPoint.trackingId, outputTestLog);
        bool validPointArea = false;
        bool validRadiusTest = false;

        if (validPointInRange)
        {
            validPointArea = test_point_area(matrices, newTargetPoint, trackedPoint.trackingId, outputTestLog);
            validRadiusTest = test_foreground_radius_percentage(matrices, newTargetPoint, trackedPoint.trackingId, outputTestLog);
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
                    trackedPoint.trackingStatus = TrackingStatus::Lost;
                    exitProbation = true;
                }
            }
            else if (trackedPoint.failedTestCount >= m_maxFailedTestsInProbation)
            {
                //failed N tests total (non-consecutive) within the probation period
                //too many failed tests, so long...
                trackedPoint.trackingStatus = TrackingStatus::Lost;
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
        PROFILE_FUNC();
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
        PROFILE_FUNC();
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
        PROFILE_FUNC();
        float referenceDepth = matrices.depth.at<float>(seedPosition);
        float referenceAreaSqrt = matrices.areaSqrt.at<float>(seedPosition);
        if (referenceDepth == 0 || referenceAreaSqrt == 0 || seedPosition == segmentation::INVALID_POINT)
        {
            //Cannot expect to properly segment when the seedPosition has zero depth
            return;
        }

        auto scalingMapper = get_scaling_mapper(matrices);
        cv::Point3f referenceWorldPosition =
                        scalingMapper.convert_depth_to_world(seedPosition.x,
                                                             seedPosition.y,
                                                             referenceDepth);

        TrackingData createTrackingData(matrices,
                                        seedPosition,
                                        referenceWorldPosition,
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

        bool outputTestLog = false;
        bool validPointInRange = test_point_in_range(matrices, targetPoint, -1, outputTestLog);

        if (!validPointInRange)
        {
            return;
        }

        bool validPointArea = test_point_area(matrices, targetPoint, -1, outputTestLog);
        bool validRadiusTest = test_foreground_radius_percentage(matrices, targetPoint, -1, outputTestLog);

        if (!validPointArea || !validRadiusTest)
        {
            return;
        }

        bool existingPoint = false;

        float depth = matrices.depth.at<float>(targetPoint);

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
