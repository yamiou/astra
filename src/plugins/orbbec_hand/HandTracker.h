#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <SenseKitUL/SenseKitUL.h>

#include "DepthUtility.h"
#include "TrackedPoint.h"
#include "PointProcessor.h"
#include <SenseKitUL/Plugins/stream_types.h>
#include "ScalingCoordinateMapper.h"
#include <SenseKit/Plugins/PluginKit.h>
#include "HandStream.h"
#include "DebugHandStream.h"
#include "DebugVisualizer.h"
#include <memory>
#include "HandSettings.h"

namespace sensekit { namespace plugins { namespace hand {

    class HandTracker : public FrameReadyListener
    {
    public:
        HandTracker(PluginServiceProxy& pluginService,
                    Sensor& streamset,
                    StreamDescription& depthDesc,
                    PluginLogger& pluginLogger,
                    HandSettings& settings);
        virtual ~HandTracker();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;
    private:
        void create_streams(PluginServiceProxy& pluginService, Sensor streamset);
        void reset();
        void generate_hand_frame(sensekit_frame_index_t frameIndex);
        static void copy_position(cv::Point3f& source, sensekit_vector3f_t& target);
        static sensekit_handstatus_t convert_hand_status(TrackingStatus status, TrackedPointType type);
        static void reset_hand_point(sensekit_handpoint_t& point);

        void overlay_circle(_sensekit_imageframe& imageFrame);
        void update_debug_image_frame(_sensekit_imageframe& sensekitColorframe);
        void generate_hand_debug_image_frame(sensekit_frame_index_t frameIndex);
        void update_tracking(DepthFrame& depthFrame);
        void update_hand_frame(std::vector<TrackedPoint>& internalTrackedPoints, _sensekit_handframe& frame);

        void track_points(cv::Mat& matDepth, cv::Mat& matDepthFullSize, cv::Mat& matForeground);
        
        //fields

        PluginServiceProxy& m_pluginService;
        PluginLogger& m_logger;
        DepthUtility m_depthUtility;
        PointProcessor m_pointProcessor;
        StreamReader m_reader;
        DepthStream m_depthStream;

        using ColorStreamPtr = std::unique_ptr <DebugHandStream> ;
        ColorStreamPtr m_debugImageStream;

        using HandStreamPtr = std::unique_ptr<HandStream>;
        HandStreamPtr m_handStream;

        int m_width;
        int m_height;

        cv::Mat m_matDepth;
        cv::Mat m_matDepthFullSize;
        cv::Mat m_matDepthWindow;
        cv::Mat m_matVelocitySignal;
        cv::Mat m_matBasicScore;
        cv::Mat m_matArea;
        cv::Mat m_matAreaSqrt;
        cv::Mat m_debugUpdateSegmentation;
        cv::Mat m_debugCreateSegmentation;
        cv::Mat m_debugRefineSegmentation;
        cv::Mat m_updateForegroundSearched;
        cv::Mat m_createForegroundSearched;
        cv::Mat m_debugUpdateScore;
        cv::Mat m_debugCreateScore;
        cv::Mat m_debugRefineScore;

        cv::Mat m_layerSegmentation;
        cv::Mat m_layerScore;
        cv::Mat m_layerEdgeDistance;

        cv::Mat m_refineForegroundSearched;
        cv::Mat m_refineSegmentation;
        cv::Mat m_refineScore;
        cv::Mat m_refineEdgeDistance;

        DebugVisualizer m_debugVisualizer;

        float m_processingSizeWidth;
        float m_processingSizeHeight;
    };

}}}

#endif // HANDTRACKER_H
