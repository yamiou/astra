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
#include "../../SenseKit/sensekit_internal.h"
#include <SenseKit/Plugins/plugin_api.h>

using namespace std;

#define GL_WIN_SIZE_X   640
#define GL_WIN_SIZE_Y   480

#define PROCESSING_SIZE_WIDTH 80
#define PROCESSING_SIZE_HEIGHT 60

#define TEXTURE_SIZE    512

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))

HandTracker::HandTracker(sensekit::PluginServiceProxy* pluginService) :
    PluginBase(pluginService),
    m_depthUtility(PROCESSING_SIZE_WIDTH, PROCESSING_SIZE_HEIGHT),
    m_converter(1.0),
    m_pointProcessor(m_converter)
{
}

HandTracker::~HandTracker()
{
}

void HandTracker::setupStream(sensekit_streamset_t setHandle, sensekit_stream_desc_t depthStreamDesc)
{
    m_streamSet = setHandle;

    stream_callbacks_t pluginCallbacks = sensekit::create_plugin_callbacks(this);

    sensekit_stream_desc_t desc;
    desc.type = SENSEKIT_STREAM_HANDS;
    desc.subType = HANDS_DEFAULT_SUBTYPE;

    get_pluginService().create_stream(m_streamSet, desc, pluginCallbacks, &m_handStream);

    //subscribe to m_depthStream's frame ready event
    sensekit_reader_t m_reader;
    sensekit_reader_create(m_streamSet, &m_reader);

    sensekit_streamconnection_t depthConnection;
    sensekit_status_t rc = sensekit_reader_get_stream(m_reader, depthStreamDesc.type, depthStreamDesc.subType, &depthConnection);
    if (rc != SENSEKIT_STATUS_SUCCESS)
    {
        sensekit_reader_destroy(&m_reader);
        throw std::logic_error("Could not add a stream with desired description");
    }

    sensekit_stream_start(depthConnection);

    sensekit_reader_register_frame_ready_callback(m_reader, &HandTracker::reader_frame_ready_thunk, this, &m_readerCallbackId);
}

void HandTracker::reader_frame_ready_thunk(sensekit_reader_t reader, sensekit_reader_frame_t frame, void* clientTag)
{
    HandTracker* tracker = static_cast<HandTracker*>(clientTag);
    tracker->reader_frame_ready(reader, frame);
}

void HandTracker::reader_frame_ready(sensekit_reader_t reader, sensekit_reader_frame_t frame)
{
    sensekit_depthframe_t depthFrame;
    sensekit_depth_frame_get(frame, &depthFrame);
    updateTracking(depthFrame);
}

void HandTracker::reset()
{
    m_depthUtility.reset();
    m_pointProcessor.reset();
}

void HandTracker::updateTracking(sensekit_depthframe_t depthFrame)
{
    sensekit_depthframe_metadata_t metadata;
    sensekit_depthframe_get_metadata(depthFrame, &metadata);

    int width = metadata.width;
    int height = metadata.height;

    m_resizeFactor = width / static_cast<float>(PROCESSING_SIZE_WIDTH);

    m_depthUtility.processDepthToForeground(depthFrame, width, height, m_matDepth, m_matForeground);

    float minArea = 10000;
    float maxArea = 20000;
    trackPoints(m_matDepth, m_matForeground);

    if (m_currentHandFrame != nullptr)
    {
        updateHandFrame(m_pointProcessor.get_trackedPoints(), m_currentHandFrame);
    }
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

    float heightFactor = 1;
    float depthFactor = 1.5;

    SegmentationUtility::calculateBasicScore(matDepth, matScore, heightFactor, depthFactor, m_resizeFactor);
    SegmentationUtility::calculateSegmentArea(matDepth, matArea, m_resizeFactor);

    cv::Mat foregroundCopy = matForeground.clone();

    TrackingMatrices matrices(matDepth, matArea, matScore, matForeground, layerSegmentation);

    m_pointProcessor.updateTrackedPoints(matrices);

    m_pointProcessor.removeDuplicatePoints();

    cv::Point seedPosition;
    //add new points (unless already tracking)
    while (SegmentationUtility::findForegroundPixel(foregroundCopy, seedPosition))
    {
        m_pointProcessor.updateTrackedPointOrCreateNewPointFromSeedPosition(matrices, seedPosition);
    }

    //remove old points
    m_pointProcessor.removeOldOrDeadPoints();
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

void HandTracker::setNextBuffer(sensekit_frame_t* nextBuffer)
{
    m_currentBuffer = nextBuffer;
    m_currentHandFrame = static_cast<sensekit_handframe_wrapper_t*>(m_currentBuffer->data);
    if (m_currentHandFrame != nullptr)
    {
        m_currentHandFrame->frame.handpoints = reinterpret_cast<sensekit_handpoint_t*>(&(m_currentHandFrame->frame_data));
        m_currentHandFrame->frame.frameIndex = m_frameIndex;
        m_currentHandFrame->frame.numHands = SENSEKIT_HANDS_MAX_HANDPOINTS;
    }
}

void HandTracker::set_parameter(sensekit_streamconnection_t streamConnection,
                                sensekit_parameter_id id,
                                size_t byteLength,
                                sensekit_parameter_data_t* data)
{
}

void HandTracker::get_parameter_size(sensekit_streamconnection_t streamConnection,
                                     sensekit_parameter_id id,
                                     size_t& byteLength)
{
    byteLength = 20;
}

void HandTracker::get_parameter_data(sensekit_streamconnection_t streamConnection,
                                     sensekit_parameter_id id,
                                     size_t byteLength,
                                     sensekit_parameter_data_t* data)
{
    char* charData = (char*)data;
    for (int i = 0; i < byteLength; i++)
    {
        charData[i] = i;
    }
}

void HandTracker::connection_added(sensekit_streamconnection_t connection)
{
    int binLength = sizeof(sensekit_handframe_t) + SENSEKIT_HANDS_MAX_HANDPOINTS * sizeof(sensekit_handpoint_t);

    sensekit_frame_t* nextBuffer = nullptr;
    get_pluginService().create_stream_bin(m_handStream, binLength, &m_handBinHandle, &nextBuffer);
    setNextBuffer(nextBuffer);
    get_pluginService().link_connection_to_bin(connection, m_handBinHandle);
}

void HandTracker::connection_removed(sensekit_streamconnection_t connection)
{
    //TODO need API for get bin client count...don't destroy if other clients assigned to it
    get_pluginService().link_connection_to_bin(connection, nullptr);
    get_pluginService().destroy_stream_bin(m_handStream, &m_handBinHandle, &m_currentBuffer);
    setNextBuffer(m_currentBuffer);
}
