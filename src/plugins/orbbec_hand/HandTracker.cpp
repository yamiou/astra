// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif

#include "HandTracker.h"
#include "Segmentation.h"
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKit/Plugins/PluginKit.h>
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace hand {

        using namespace std;

        HandTracker::HandTracker(PluginServiceProxy& pluginService,
                                 sensekit_streamset_t streamSet,
                                 StreamDescription& depthDesc,
                                 HandSettings& settings) :
            m_pluginService(pluginService),
            m_depthUtility(settings.processingSizeWidth, settings.processingSizeHeight, settings.depthUtilitySettings),
            m_pointProcessor(settings.pointProcessorSettings),
            m_processingSizeWidth(settings.processingSizeWidth),
            m_processingSizeHeight(settings.processingSizeHeight)
        {
            PROFILE_FUNC();

            const char* uri;
            pluginService.get_streamset_uri(streamSet, &uri);

            m_sensor = Sensor(uri);
            m_reader = m_sensor.create_reader();
            create_streams(m_pluginService, streamSet);

            m_depthStream = m_reader.stream<DepthStream>(depthDesc.get_subtype());
            m_depthStream.start();

            m_reader.stream<PointStream>().start();

            m_reader.addListener(*this);
        }

        HandTracker::~HandTracker()
        {
            PROFILE_FUNC();
            if (m_worldPoints != nullptr)
            {
                delete[] m_worldPoints;
                m_worldPoints = nullptr;
            }
        }

        void HandTracker::create_streams(PluginServiceProxy& pluginService, sensekit_streamset_t streamSet)
        {
            PROFILE_FUNC();
            SINFO("HandTracker", "creating hand streams");
            m_handStream = make_unique<HandStream>(pluginService, streamSet, SENSEKIT_HANDS_MAX_HAND_COUNT);

            const int bytesPerPixel = 3;

            m_debugImageStream = make_unique<DebugHandStream>(pluginService,
                                                              streamSet,
                                                              m_processingSizeWidth,
                                                              m_processingSizeHeight,
                                                              bytesPerPixel);
        }

        void HandTracker::on_frame_ready(StreamReader& reader, Frame& frame)
        {
            PROFILE_FUNC();
            if (m_handStream->has_connections() ||
                m_debugImageStream->has_connections())
            {
                DepthFrame depthFrame = frame.get<DepthFrame>();
                PointFrame pointFrame = frame.get<PointFrame>();
                update_tracking(depthFrame, pointFrame);
            }

            PROFILE_UPDATE();
        }

        void HandTracker::reset()
        {
            PROFILE_FUNC();
            m_depthUtility.reset();
            m_pointProcessor.reset();
        }

        void HandTracker::update_tracking(DepthFrame& depthFrame, PointFrame& pointFrame)
        {
            PROFILE_FUNC();
            if (!m_debugImageStream->pause_input())
            {
                m_depthUtility.processDepthToVelocitySignal(depthFrame, m_matDepth, m_matDepthFullSize, m_matVelocitySignal);
            }

            track_points(m_matDepth, m_matDepthFullSize, m_matVelocitySignal, pointFrame.data());

            //use same frameIndex as source depth frame
            sensekit_frame_index_t frameIndex = depthFrame.frameIndex();

            if (m_handStream->has_connections())
            {
                generate_hand_frame(frameIndex);
            }

            if (m_debugImageStream->has_connections())
            {
                generate_hand_debug_image_frame(frameIndex);
            }
        }

        void HandTracker::track_points(cv::Mat& matDepth,
                                       cv::Mat& matDepthFullSize,
                                       cv::Mat& matVelocitySignal,
                                       const Vector3f* fullSizeWorldPoints)
        {
            PROFILE_FUNC();

            m_layerSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_layerScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_layerEdgeDistance = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugUpdateSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_debugCreateSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_debugRefineSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_updateForegroundSearched = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_createForegroundSearched = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_refineForegroundSearched = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_debugUpdateScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugCreateScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugTestPassMap = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_matDepthWindow = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_refineSegmentation = cv::Mat::zeros(matDepth.size(), CV_8UC1);
            m_refineScore = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_refineEdgeDistance = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugUpdateScoreValue = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugCreateScoreValue = cv::Mat::zeros(matDepth.size(), CV_32FC1);
            m_debugRefineScoreValue = cv::Mat::zeros(matDepth.size(), CV_32FC1);

            int numPoints = matDepth.cols * matDepth.rows;
            if (m_worldPoints == nullptr || m_numWorldPoints != numPoints)
            {
                if (m_worldPoints != nullptr)
                {
                    delete[] m_worldPoints;
                    m_worldPoints = nullptr;
                }

                m_numWorldPoints = numPoints;
                m_worldPoints = new sensekit::Vector3f[numPoints];
            }

            const conversion_cache_t depthToWorldData = m_depthStream.depth_to_world_data();

            bool debugLayersEnabled = m_debugImageStream->has_connections();
            bool enabledTestPassMap = m_debugImageStream->view_type() == DEBUG_HAND_VIEW_TEST_PASS_MAP;

            TrackingMatrices updateMatrices(matDepthFullSize,
                                            matDepth,
                                            m_matArea,
                                            m_matAreaSqrt,
                                            m_matBasicScore,
                                            matVelocitySignal,
                                            m_updateForegroundSearched,
                                            m_layerSegmentation,
                                            m_layerScore,
                                            m_layerEdgeDistance,
                                            m_debugUpdateSegmentation,
                                            m_debugUpdateScore,
                                            m_debugUpdateScoreValue,
                                            m_debugTestPassMap,
                                            enabledTestPassMap,
                                            fullSizeWorldPoints,
                                            m_worldPoints,
                                            debugLayersEnabled,
                                            m_depthStream.coordinateMapper(),
                                            depthToWorldData);

            if (!m_debugImageStream->pause_input())
            {
                m_pointProcessor.initialize_common_calculations(updateMatrices);
            }

            //Update existing points first so that if we lose a point, we might recover it in the "add new" stage below
            //without having at least one frame of a lost point.

            m_pointProcessor.updateTrackedPoints(updateMatrices);

            m_pointProcessor.removeDuplicatePoints();

            TrackingMatrices createMatrices(matDepthFullSize,
                                            matDepth,
                                            m_matArea,
                                            m_matAreaSqrt,
                                            m_matBasicScore,
                                            matVelocitySignal,
                                            m_createForegroundSearched,
                                            m_layerSegmentation,
                                            m_layerScore,
                                            m_layerEdgeDistance,
                                            m_debugCreateSegmentation,
                                            m_debugCreateScore,
                                            m_debugCreateScoreValue,
                                            m_debugTestPassMap,
                                            enabledTestPassMap,
                                            fullSizeWorldPoints,
                                            m_worldPoints,
                                            debugLayersEnabled,
                                            m_depthStream.coordinateMapper(),
                                            depthToWorldData);

            //add new points (unless already tracking)
            if (!m_debugImageStream->use_mouse_probe())
            {
                cv::Point seedPosition;
                cv::Point nextSearchStart(0, 0);
                while (segmentation::find_next_velocity_seed_pixel(matVelocitySignal, m_createForegroundSearched, seedPosition, nextSearchStart))
                {
                    m_pointProcessor.updateTrackedPointOrCreateNewPointFromSeedPosition(createMatrices, seedPosition);
                }
            }
            else
            {
                debug_probe_point(createMatrices);
            }

            debug_spawn_point(createMatrices);

            //remove old points
            m_pointProcessor.removeOldOrDeadPoints();

            TrackingMatrices refinementMatrices(matDepthFullSize,
                                                m_matDepthWindow,
                                                m_matArea,
                                                m_matAreaSqrt,
                                                m_matBasicScore,
                                                matVelocitySignal,
                                                m_refineForegroundSearched,
                                                m_refineSegmentation,
                                                m_refineScore,
                                                m_refineEdgeDistance,
                                                m_debugRefineSegmentation,
                                                m_debugRefineScore,
                                                m_debugRefineScoreValue,
                                                m_debugTestPassMap,
                                                enabledTestPassMap,
                                                fullSizeWorldPoints,
                                                m_worldPoints,
                                                false,
                                                m_depthStream.coordinateMapper(),
                                                depthToWorldData);

            m_pointProcessor.update_full_resolution_points(refinementMatrices);

            m_pointProcessor.update_trajectories();
        }

        void HandTracker::debug_probe_point(TrackingMatrices& matrices)
        {
            if (!m_debugImageStream->use_mouse_probe())
            {
                return;
            }

            cv::Point seedPosition = get_mouse_probe_position();
            m_pointProcessor.updateTrackedPointOrCreateNewPointFromSeedPosition(matrices, seedPosition);

            cv::Mat& matDepth = matrices.depth;

            float area = m_pointProcessor.get_point_area(matrices, seedPosition);
            float depth = matDepth.at<float>(seedPosition);
            float score = m_debugCreateScoreValue.at<float>(seedPosition);
            float edgeDist = m_layerEdgeDistance.at<float>(seedPosition);

            float foregroundRadius1 = m_pointProcessor.foregroundRadius1();
            float foregroundRadius2 = m_pointProcessor.foregroundRadius2();

            auto mapper = get_scaling_mapper(matrices);
            float percentForeground1 = segmentation::get_percent_foreground_along_circumference(matDepth,
                                                                                               m_layerSegmentation,
                                                                                               seedPosition,
                                                                                               foregroundRadius1,
                                                                                               mapper);
            float percentForeground2 = segmentation::get_percent_foreground_along_circumference(matDepth,
                                                                                               m_layerSegmentation,
                                                                                               seedPosition,
                                                                                               foregroundRadius2,
                                                                                               mapper);
            SINFO("HandTracker", "depth: %f fg1: %f fg2: %f edge: %f area: %f score: %f", depth,
                                                                    percentForeground1,
                                                                    percentForeground2,
                                                                    edgeDist,
                                                                    area,
                                                                    score);
        }

        void HandTracker::debug_spawn_point(TrackingMatrices& matrices)
        {
            if (!m_debugImageStream->spawn_point_requested())
            {
                return;
            }
            if (!m_debugImageStream->pause_input())
            {
                m_pointProcessor.initialize_common_calculations(matrices);
            }
            cv::Point seedPosition = get_mouse_probe_position();

            m_pointProcessor.updateTrackedPointOrCreateNewPointFromSeedPosition(matrices, seedPosition);

            m_pointProcessor.calculateTestPassMap(matrices);

            bool outputTestLog = true;

            bool validPointInRange = m_pointProcessor.test_point_in_range(matrices,
                                                                          seedPosition,
                                                                          0,
                                                                          outputTestLog);
            bool validPointArea = false;
            bool validRadiusTest = false;

            if (validPointInRange)
            {
                validPointArea = m_pointProcessor.test_point_area(matrices,
                                                                  seedPosition,
                                                                  0,
                                                                  outputTestLog);
                validRadiusTest = m_pointProcessor.test_foreground_radius_percentage(matrices,
                                                                                     seedPosition,
                                                                                     0,
                                                                                     outputTestLog);
            }

            float depth = matrices.depth.at<float>(seedPosition);
            float score = m_debugCreateScoreValue.at<float>(seedPosition);
            SINFO("HandTracker", "point test: depth: %f score: %f inRange: %d validArea: %d validRadius: %d",
                          depth,
                          score,
                          validPointInRange,
                          validPointArea,
                          validRadiusTest);

            m_debugImageStream->clear_spawn_point_request();
        }

        cv::Point HandTracker::get_mouse_probe_position()
        {
            auto normPosition = m_debugImageStream->mouse_norm_position();
            int x = MAX(0, MIN(m_processingSizeWidth, normPosition.x * m_processingSizeWidth));
            int y = MAX(0, MIN(m_processingSizeHeight, normPosition.y * m_processingSizeHeight));
            return cv::Point(x, y);
        }

        void HandTracker::generate_hand_frame(sensekit_frame_index_t frameIndex)
        {
            PROFILE_FUNC();

            sensekit_handframe_wrapper_t* handFrame = m_handStream->begin_write(frameIndex);

            if (handFrame != nullptr)
            {
                handFrame->frame.handpoints = reinterpret_cast<sensekit_handpoint_t*>(&(handFrame->frame_data));
                handFrame->frame.handCount = SENSEKIT_HANDS_MAX_HAND_COUNT;

                update_hand_frame(m_pointProcessor.get_trackedPoints(), handFrame->frame);

                PROFILE_BEGIN(end_write);
                m_handStream->end_write();
                PROFILE_END();
            }
        }

        void HandTracker::generate_hand_debug_image_frame(sensekit_frame_index_t frameIndex)
        {
            PROFILE_FUNC();
            sensekit_imageframe_wrapper_t* debugImageFrame = m_debugImageStream->begin_write(frameIndex);

            if (debugImageFrame != nullptr)
            {
                debugImageFrame->frame.data = reinterpret_cast<uint8_t *>(&(debugImageFrame->frame_data));

                sensekit_image_metadata_t metadata;

                metadata.width = m_processingSizeWidth;
                metadata.height = m_processingSizeHeight;
                metadata.bytesPerPixel = 3;

                debugImageFrame->frame.metadata = metadata;
                update_debug_image_frame(debugImageFrame->frame);

                m_debugImageStream->end_write();
            }
        }

        void HandTracker::update_hand_frame(vector<TrackedPoint>& internalTrackedPoints, _sensekit_handframe& frame)
        {
            PROFILE_FUNC();
            int handIndex = 0;
            int maxHandCount = frame.handCount;

            bool includeCandidates = m_handStream->include_candidate_points();

            for (auto it = internalTrackedPoints.begin(); it != internalTrackedPoints.end(); ++it)
            {
                TrackedPoint internalPoint = *it;

                TrackingStatus status = internalPoint.trackingStatus;
                TrackedPointType pointType = internalPoint.pointType;

                bool includeByStatus = status == Tracking ||
                                       status == Lost;
                bool includeByType = pointType == ActivePoint ||
                                     (pointType == CandidatePoint && includeCandidates);
                if (includeByStatus && includeByType && handIndex < maxHandCount)
                {
                    sensekit_handpoint_t& point = frame.handpoints[handIndex];
                    ++handIndex;

                    point.trackingId = internalPoint.trackingId;

                    point.depthPosition.x = internalPoint.fullSizePosition.x;
                    point.depthPosition.y = internalPoint.fullSizePosition.y;

                    copy_position(internalPoint.fullSizeWorldPosition, point.worldPosition);
                    copy_position(internalPoint.fullSizeWorldDeltaPosition, point.worldDeltaPosition);

                    point.status = convert_hand_status(status, pointType);
                }
            }
            for (int i = handIndex; i < maxHandCount; ++i)
            {
                sensekit_handpoint_t& point = frame.handpoints[i];
                reset_hand_point(point);
            }
        }

        void HandTracker::copy_position(cv::Point3f& source, sensekit_vector3f_t& target)
        {
            PROFILE_FUNC();
            target.x = source.x;
            target.y = source.y;
            target.z = source.z;
        }

        sensekit_handstatus_t HandTracker::convert_hand_status(TrackingStatus status, TrackedPointType type)
        {
            PROFILE_FUNC();
            if (type == TrackedPointType::CandidatePoint)
            {
                return HAND_STATUS_CANDIDATE;
            }
            switch (status)
            {
            case Tracking:
                return HAND_STATUS_TRACKING;
                break;
            case Lost:
                return HAND_STATUS_LOST;
                break;
            case Dead:
            case NotTracking:
            default:
                return HAND_STATUS_NOTTRACKING;
                break;
            }
        }

        void HandTracker::reset_hand_point(sensekit_handpoint_t& point)
        {
            PROFILE_FUNC();
            point.trackingId = -1;
            point.status = HAND_STATUS_NOTTRACKING;
            point.depthPosition = sensekit_vector2i_t();
            point.worldPosition = sensekit_vector3f_t();
            point.worldDeltaPosition = sensekit_vector3f_t();
        }

        void mark_image_pixel(_sensekit_imageframe& imageFrame,
                              RGBPixel color,
                              cv::Point p)
        {
            PROFILE_FUNC();
            RGBPixel* colorData = static_cast<RGBPixel*>(imageFrame.data);
            int index = p.x + p.y * imageFrame.metadata.width;
            colorData[index] = color;
        }

        void HandTracker::overlay_circle(_sensekit_imageframe& imageFrame)
        {
            PROFILE_FUNC();
            auto normPosition = m_debugImageStream->mouse_norm_position();
            int x = MAX(0, MIN(m_processingSizeWidth, normPosition.x * m_processingSizeWidth));
            int y = MAX(0, MIN(m_processingSizeHeight, normPosition.y * m_processingSizeHeight));

            float resizeFactor = m_matDepthFullSize.cols / static_cast<float>(m_matDepth.cols);
            ScalingCoordinateMapper mapper(m_depthStream.coordinateMapper(), resizeFactor);

            RGBPixel color(255, 0, 255);

            auto callback = [&](cv::Point p)
            {
                mark_image_pixel(imageFrame, color, p);
            };

            float foregroundRadius1 = m_pointProcessor.foregroundRadius1();
            float foregroundRadius2 = m_pointProcessor.foregroundRadius2();

            segmentation::visit_circle_circumference(m_matDepth, cv::Point(x, y), foregroundRadius1, mapper, callback);
            segmentation::visit_circle_circumference(m_matDepth, cv::Point(x, y), foregroundRadius2, mapper, callback);
        }

        void HandTracker::update_debug_image_frame(_sensekit_imageframe& colorFrame)
        {
            PROFILE_FUNC();
            float m_maxVelocity = 0.1;

            RGBPixel foregroundColor(0, 0, 255);
            RGBPixel searchedColor(128, 255, 0);
            RGBPixel searchedColor2(0, 128, 255);
            RGBPixel testPassColor(0, 255, 128);

            DebugHandViewType view = m_debugImageStream->view_type();

            switch (view)
            {
            case DEBUG_HAND_VIEW_DEPTH:
                m_debugVisualizer.showDepthMat(m_matDepth,
                                               colorFrame);
                break;
            case DEBUG_HAND_VIEW_DEPTH_MOD:
                m_debugVisualizer.showDepthMat(m_depthUtility.matDepthFilled(),
                                               colorFrame);
                break;
            case DEBUG_HAND_VIEW_DEPTH_AVG:
                m_debugVisualizer.showDepthMat(m_depthUtility.matDepthAvg(),
                                               colorFrame);
                break;
            case DEBUG_HAND_VIEW_VELOCITY:
                m_debugVisualizer.showVelocityMat(m_depthUtility.matDepthVel(),
                                                  m_maxVelocity,
                                                  colorFrame);
                break;
            case DEBUG_HAND_VIEW_FILTEREDVELOCITY:
                m_debugVisualizer.showVelocityMat(m_depthUtility.matDepthVelErode(),
                                                  m_maxVelocity,
                                                  colorFrame);
                break;
            case DEBUG_HAND_VIEW_UPDATE_SEGMENTATION:
                m_debugVisualizer.showNormArray<char>(m_debugUpdateSegmentation,
                                                      m_debugUpdateSegmentation,
                                                      colorFrame);
                break;
            case DEBUG_HAND_VIEW_CREATE_SEGMENTATION:
                m_debugVisualizer.showNormArray<char>(m_debugCreateSegmentation,
                                                      m_debugCreateSegmentation,
                                                      colorFrame);
                break;
            case DEBUG_HAND_VIEW_UPDATE_SEARCHED:
            case DEBUG_HAND_VIEW_CREATE_SEARCHED:
                m_debugVisualizer.showDepthMat(m_matDepth,
                                               colorFrame);
                break;
            case DEBUG_HAND_VIEW_CREATE_SCORE:
                m_debugVisualizer.showNormArray<float>(m_debugCreateScore,
                                                       m_debugCreateSegmentation,
                                                       colorFrame);
                break;
            case DEBUG_HAND_VIEW_UPDATE_SCORE:
                m_debugVisualizer.showNormArray<float>(m_debugUpdateScore,
                                                       m_debugUpdateSegmentation,
                                                       colorFrame);
                break;
            case DEBUG_HAND_VIEW_HANDWINDOW:
                m_debugVisualizer.showDepthMat(m_matDepthWindow,
                                               colorFrame);
                break;
            case DEBUG_HAND_VIEW_TEST_PASS_MAP:
                m_debugVisualizer.showDepthMat(m_matDepth,
                                               colorFrame);
                m_debugVisualizer.overlayMask(m_debugTestPassMap,
                                              colorFrame,
                                              testPassColor,
                                              PixelType::Foreground);
                break;
            }

            if (view != DEBUG_HAND_VIEW_HANDWINDOW &&
                view != DEBUG_HAND_VIEW_CREATE_SCORE &&
                view != DEBUG_HAND_VIEW_UPDATE_SCORE &&
                view != DEBUG_HAND_VIEW_DEPTH_MOD &&
                view != DEBUG_HAND_VIEW_DEPTH_AVG &&
                view != DEBUG_HAND_VIEW_TEST_PASS_MAP)
            {
                if (view == DEBUG_HAND_VIEW_CREATE_SEARCHED)
                {
                    m_debugVisualizer.overlayMask(m_createForegroundSearched, colorFrame, searchedColor, PixelType::Searched);
                    m_debugVisualizer.overlayMask(m_createForegroundSearched, colorFrame, searchedColor2, PixelType::SearchedFromOutOfRange);
                }
                else if (view == DEBUG_HAND_VIEW_UPDATE_SEARCHED)
                {
                    m_debugVisualizer.overlayMask(m_updateForegroundSearched, colorFrame, searchedColor, PixelType::Searched);
                    m_debugVisualizer.overlayMask(m_updateForegroundSearched, colorFrame, searchedColor2, PixelType::SearchedFromOutOfRange);
                }

                m_debugVisualizer.overlayMask(m_matVelocitySignal, colorFrame, foregroundColor, PixelType::Foreground);
            }

            if (m_debugImageStream->use_mouse_probe())
            {
                overlay_circle(colorFrame);
            }
        }
}}}
