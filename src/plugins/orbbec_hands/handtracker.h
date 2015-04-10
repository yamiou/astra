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
#include <SenseKit/Plugins/StreamCallbackListener.h>
#include <SenseKit/Plugins/PluginStream.h>

namespace sensekit
{
    namespace plugins
    {
        namespace hands
        {
            class HandTracker
            {
            public:
                HandTracker(PluginServiceProxy& pluginService,
                            sensekit_streamset_t setHandle,
                            sensekit_stream_desc_t depthStreamDesc);
                virtual ~HandTracker();

            private:
                void create_streams(PluginServiceProxy& pluginService, sensekit_streamset_t setHandle);
                void subscribeToDepthStream(sensekit_streamset_t setHandle, sensekit_stream_desc_t depthStreamDesc);
                void reset();
                static void copyPosition(cv::Point3f& source, sensekit_vector3f_t& target);
                static sensekit_handstatus_t convertHandStatus(TrackingStatus status);
                static void resetHandPoint(sensekit_handpoint_t& point);

                void updateTracking(sensekit_depthframe_t depthFrame);
                void updateHandFrame(std::vector<TrackedPoint>& internalTrackedPoints, _sensekit_handframe& frame);

                void trackPoints(cv::Mat& matDepth, cv::Mat& matForeground);

                static void reader_frame_ready_thunk(void* clientTag, sensekit_reader_t reader, sensekit_reader_frame_t frame);
                void reader_frame_ready(sensekit_reader_t reader, sensekit_reader_frame_t frame);
                
                //fields
                PluginServiceProxy& m_pluginService;
                PluginStream<sensekit_handframe_wrapper_t>* m_handStream;

                sensekit_reader_t m_reader{ nullptr };
                sensekit_frame_ready_callback_t m_readerFrameReadyCallback{ nullptr };
                sensekit_reader_callback_id_t m_readerCallbackId { nullptr };

                DepthUtility m_depthUtility;
                CoordinateConverter m_converter;
                PointProcessor m_pointProcessor;

                int m_width;
                int m_height;

                float m_resizeFactor;

                cv::Mat m_matDepth;
                cv::Mat m_matForeground;

            };
        }
    }
}

#endif // HANDTRACKER_H
