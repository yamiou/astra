#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL.h>

#include "depthutility.h"
#include "trackedpoint.h"
#include "pointprocessor.h"
#include "../../SenseKitUL/SenseKitUL_internal.h"
#include "coordinateconverter.h"
#include <SenseKit/Plugins/plugin_api.h>

class HandTracker : public sensekit::PluginBase
{
public:
    HandTracker(sensekit::PluginServiceProxy* pluginService);
    virtual ~HandTracker();

    void setupStream(sensekit_streamset_t setHandle, sensekit_stream_desc_t depthStreamDesc);
private:
    void reset();
    static void copyPosition(cv::Point3f& source, sensekit_vector3f_t& target);
    static sensekit_handstatus_t convertHandStatus(TrackingStatus status);
    static void resetHandPoint(sensekit_handpoint_t& point);

    void updateTracking(sensekit_depthframe_t depthFrame);
    void updateHandFrame(std::vector<TrackedPoint>& internalTrackedPoints, sensekit_handframe_wrapper_t* handframe_wrapper);

    void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);
    
    static void reader_frame_ready_thunk(void* clientTag, sensekit_reader_t reader, sensekit_reader_frame_t frame);
    void reader_frame_ready(sensekit_reader_t reader, sensekit_reader_frame_t frame);
    void setNextBuffer(sensekit_frame_t* sensekitFrame);

    virtual void set_parameter(sensekit_streamconnection_t connection,
                               sensekit_parameter_id id,
                               size_t byteLength,
                               sensekit_parameter_data_t* data) override;

    virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    size_t& byteLength) override;

    virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    size_t byteLength,
                                    sensekit_parameter_data_t* data) override;

    virtual void connection_added(sensekit_streamconnection_t connection) override;
    virtual void connection_removed(sensekit_streamconnection_t connection) override;

    //fields
    sensekit_streamset_t m_streamSet { nullptr };
    sensekit_reader_t m_reader { nullptr };
    sensekit_frame_ready_callback_t m_readerFrameReadyCallback { nullptr };
    sensekit_reader_callback_id_t m_readerCallbackId;

    DepthUtility m_depthUtility;
    CoordinateConverter m_converter;
    PointProcessor m_pointProcessor;

    sensekit_stream_t m_handStream{nullptr};
    sensekit_bin_t m_handBinHandle{nullptr};

    int m_width;
    int m_height;

    float m_resizeFactor;

    cv::Mat m_matDepth;
    cv::Mat m_matForeground;
    int m_frameIndex{ 0 };
    sensekit_frame_t* m_currentHandBuffer{ nullptr };
    sensekit_handframe_wrapper_t* m_currentHandFrame { nullptr };
};


#endif // HANDTRACKER_H
