// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif


#include "handtracker.h"
#include "depthutility.h"
#include "segmentationutility.h"
#include "coordinateconverter.h"
#include "pointprocessor.h"
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKit/Plugins/PluginKit.h>

namespace sensekit
{
    namespace plugins
    {
        namespace hands
        {
            using namespace std;

#define GL_WIN_SIZE_X   640
#define GL_WIN_SIZE_Y   480

            const int PROCESSING_SIZE_WIDTH = 80;
            const int PROCESSING_SIZE_HEIGHT = 60;

#define TEXTURE_SIZE    512

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))

            HandTracker::HandTracker(PluginServiceProxy& pluginService,
                                     Sensor& streamset,
                                     StreamDescription& depthDescription) :
                m_pluginService(pluginService),
                m_depthUtility(PROCESSING_SIZE_WIDTH, PROCESSING_SIZE_HEIGHT),
                m_reader(streamset.create_reader()),
                m_depthStream(nullptr)
            {
                create_streams(pluginService, streamset);

                subscribe_to_depth_stream(streamset, depthDescription);
            }

            HandTracker::~HandTracker()
            {
            }

            void HandTracker::on_frame_ready(StreamReader& reader, Frame& frame)
            {
                if (m_handStream->has_connections() ||
                    m_debugImageStream->has_connections())
                {
                    DepthFrame depthFrame = frame.get<DepthFrame>();

                    update_tracking(depthFrame);
                }
            }

            void HandTracker::create_streams(PluginServiceProxy& pluginService, Sensor streamset)
            {
                m_handStream = make_unique<HandStream>(pluginService, streamset, SENSEKIT_HANDS_MAX_HANDPOINTS);

                size_t debugImageByteLength = PROCESSING_SIZE_WIDTH * PROCESSING_SIZE_HEIGHT * 3;
                StreamDescription debugImageDescription(SENSEKIT_STREAM_HAND_DEBUG_IMAGE);

                m_debugImageStream = make_unique<ColorStream>(pluginService,
                                                              streamset,
                                                              debugImageDescription,
                                                              debugImageByteLength);
            }

            void HandTracker::subscribe_to_depth_stream(Sensor& streamset, StreamDescription& depthDescription)
            {
                m_depthStream = m_reader.stream<DepthStream>(depthDescription.get_subType());
                m_depthStream.start();
                m_converter = std::make_unique<CoordinateConverter>(m_depthStream, 1.0f);
                m_pointProcessor = std::make_unique<PointProcessor>(*(m_converter.get()));

                m_reader.addListener(*this);
            }

            void HandTracker::reset()
            {
                m_depthUtility.reset();
                m_pointProcessor.reset();
            }

            void HandTracker::update_tracking(DepthFrame& depthFrame)
            {
                int width = depthFrame.get_resolutionX();

                m_resizeFactor = width / static_cast<float>(PROCESSING_SIZE_WIDTH);

                m_depthUtility.processDepthToForeground(depthFrame, m_matDepth, m_matForeground);

                float minArea = 10000;
                float maxArea = 20000;
                track_points(m_matDepth, m_matForeground);

                //use same frameIndex as source depth frame
                sensekit_frame_index_t frameIndex = depthFrame.get_frameIndex();

                if (m_handStream->has_connections())
                {
                    generate_hand_frame(frameIndex);
                }

                if (m_debugImageStream->has_connections())
                {
                    generate_hand_debug_image_frame(frameIndex);
                }
            }

            void HandTracker::generate_hand_frame(sensekit_frame_index_t frameIndex)
            {
                sensekit_handframe_wrapper_t* handFrame = m_handStream->begin_write(frameIndex);

                if (handFrame != nullptr)
                {
                    handFrame->frame.handpoints = reinterpret_cast<sensekit_handpoint_t*>(&(handFrame->frame_data));
                    handFrame->frame.numHands = SENSEKIT_HANDS_MAX_HANDPOINTS;

                    update_hand_frame(m_pointProcessor->get_trackedPoints(), handFrame->frame);

                    m_handStream->end_write();
                }
            }

            void HandTracker::generate_hand_debug_image_frame(sensekit_frame_index_t frameIndex)
            {
                sensekit_colorframe_wrapper_t* debugImageFrame = m_debugImageStream->begin_write(frameIndex);

                if (debugImageFrame != nullptr)
                {
                    debugImageFrame->frame.data = reinterpret_cast<uint8_t *>(&(debugImageFrame->frame_data));

                    sensekit_colorframe_metadata_t metadata;

                    metadata.width = PROCESSING_SIZE_WIDTH;
                    metadata.height = PROCESSING_SIZE_HEIGHT;
                    metadata.bytesPerPixel = 3;

                    debugImageFrame->frame.metadata = metadata;
                    update_debug_image_frame(debugImageFrame->frame);

                    m_debugImageStream->end_write();
                }
            }

            void HandTracker::track_points(cv::Mat& matDepth, cv::Mat& matForeground)
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

                float heightFactor = 1;
                float depthFactor = 1.5;

                SegmentationUtility::calculateBasicScore(matDepth, matScore, heightFactor, depthFactor, *(m_converter.get()));
                SegmentationUtility::calculateSegmentArea(matDepth, matArea, *(m_converter.get()));

                cv::Mat foregroundCopy = matForeground.clone();

                TrackingMatrices matrices(matDepth, matArea, matScore, matForeground, layerSegmentation);

                m_pointProcessor->updateTrackedPoints(matrices);

                m_pointProcessor->removeDuplicatePoints();

                cv::Point seedPosition;
                //add new points (unless already tracking)
                while (SegmentationUtility::findForegroundPixel(foregroundCopy, seedPosition))
                {
                    m_pointProcessor->updateTrackedPointOrCreateNewPointFromSeedPosition(matrices, seedPosition);
                }

                //remove old points
                m_pointProcessor->removeOldOrDeadPoints();
            }

            void HandTracker::update_hand_frame(vector<TrackedPoint>& internalTrackedPoints, _sensekit_handframe& frame)
            {
                int handIndex = 0;
                int maxNumHands = frame.numHands;

                for (auto it = internalTrackedPoints.begin(); it != internalTrackedPoints.end(); ++it)
                {
                    TrackedPoint internalPoint = *it;

                    TrackingStatus status = internalPoint.m_status;
                    if (//internalPoint.m_type == TrackedPointType::ActivePoint &&
                        status != Dead && handIndex < maxNumHands)
                    {
                        sensekit_handpoint_t& point = frame.handpoints[handIndex];
                        ++handIndex;

                        point.trackingId = internalPoint.m_trackingId;

                        //convert from internal depth resolution to original depth resolution
                        point.depthPosition.x = internalPoint.m_position.x * m_resizeFactor;
                        point.depthPosition.y = internalPoint.m_position.y * m_resizeFactor;

                        copy_position(internalPoint.m_worldPosition, point.worldPosition);
                        copy_position(internalPoint.m_worldDeltaPosition, point.worldDeltaPosition);

                        point.status = convert_hand_status(status);
                    }
                }
                for (int i = handIndex; i < maxNumHands; ++i)
                {
                    sensekit_handpoint_t& point = frame.handpoints[i];
                    reset_hand_point(point);
                }
            }

            void HandTracker::copy_position(cv::Point3f& source, sensekit_vector3f_t& target)
            {
                target.x = source.x;
                target.y = source.y;
                target.z = source.z;
            }

            sensekit_handstatus_t HandTracker::convert_hand_status(TrackingStatus status)
            {
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
                point.trackingId = -1;
                point.status = HAND_STATUS_NOTTRACKING;
                point.depthPosition = sensekit_vector2i_t();
                point.worldPosition = sensekit_vector3f_t();
                point.worldDeltaPosition = sensekit_vector3f_t();
            }

            void HandTracker::update_debug_image_frame(_sensekit_colorframe& colorFrame)
            {
                m_debugVisualizer.showDepthMat(m_matDepth,
                                               m_matForeground,
                                               m_pointProcessor->get_trackedPoints(),
                                               colorFrame);
            }
        }
    }
}