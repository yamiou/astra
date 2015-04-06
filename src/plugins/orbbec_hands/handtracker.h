#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL.h>

#include "depthutility.h"
#include "trackedpoint.h"
#include "pointprocessor.h"
#include "../../SenseKitUL/SenseKitUL_internal.h"
#include "coordinateconverter.h"
#include <SenseKit/Plugins/PluginServiceProxy.h>
#include <SenseKit/Plugins/PluginBase.h>

class HandTracker : public sensekit::PluginBase
{
public:
    HandTracker(sensekit::PluginServiceProxy* pluginService,
                sensekit_streamset_t setHandle,
                sensekit_depthstream_t* depthStream);

    virtual ~HandTracker();
private:

    void reset();
    static void copyPosition(cv::Point3f& source, sensekit_vector3f_t& target);
    static sensekit_handstatus_t convertHandStatus(TrackingStatus status);
    static void resetHandPoint(sensekit_handpoint_t& point);

    void updateTracking(sensekit_depthframe_t* depthFrame);
    void updateHandFrame(std::vector<TrackedPoint>& internalTrackedPoints, sensekit_handframe_wrapper_t* handframe_wrapper);

    void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);
    void setupStream();

    void setNextBuffer(sensekit_frame_t* sensekitFrame);

    virtual void set_parameter(sensekit_streamconnection_t* connection,
                               sensekit_parameter_id id,
                               size_t byteLength,
                               sensekit_parameter_data_t* data) override;

    virtual void get_parameter_size(sensekit_streamconnection_t* connection,
                                    sensekit_parameter_id id,
                                    size_t& byteLength) override;

    virtual void get_parameter_data(sensekit_streamconnection_t* connection,
                                    sensekit_parameter_id id,
                                    size_t byteLength,
                                    sensekit_parameter_data_t* data) override;

    virtual void connection_added(sensekit_streamconnection_t* connection) override;
    virtual void connection_removed(sensekit_streamconnection_t* connection) override;

    //fields
    sensekit_streamset_t m_setHandle;
    sensekit_depthstream_t* m_depthStream;
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
    sensekit_frame_t* m_currentBuffer{ nullptr };
    sensekit_handframe_wrapper_t* m_currentHandFrame { nullptr };
};


#endif // HANDTRACKER_H
