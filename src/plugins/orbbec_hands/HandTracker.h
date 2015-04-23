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

namespace sensekit { namespace plugins { namespace hands {

    class HandTracker : public FrameReadyListener
    {
    public:
        HandTracker(PluginServiceProxy& pluginService,
                    Sensor& streamset,
                    StreamDescription& depthDescription);
        virtual ~HandTracker();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;
    private:
        void create_streams(PluginServiceProxy& pluginService, Sensor streamset);
        void subscribe_to_depth_stream(Sensor& streamset, StreamDescription& depthDescription);
        void reset();
        void generate_hand_frame(sensekit_frame_index_t frameIndex);
        static void copy_position(cv::Point3f& source, sensekit_vector3f_t& target);
        static sensekit_handstatus_t convert_hand_status(TrackingStatus status, TrackedPointType type);
        static void reset_hand_point(sensekit_handpoint_t& point);

        void update_debug_image_frame(_sensekit_imageframe& sensekitColorframe);
        void generate_hand_debug_image_frame(sensekit_frame_index_t frameIndex);
        void update_tracking(DepthFrame& depthFrame);
        void update_hand_frame(std::vector<TrackedPoint>& internalTrackedPoints, _sensekit_handframe& frame);

        void track_points(cv::Mat& matDepth, cv::Mat& matForeground);

        //fields

        DepthStream m_depthStream;
        PluginServiceProxy& m_pluginService;

        using ColorStreamPtr = std::unique_ptr <DebugHandStream> ;
        ColorStreamPtr m_debugImageStream;

        using HandStreamPtr = std::unique_ptr<HandStream>;
        HandStreamPtr m_handStream;

        StreamReader m_reader;

        DepthUtility m_depthUtility;
        std::unique_ptr<ScalingCoordinateMapper> m_mapper;
        std::unique_ptr<PointProcessor> m_pointProcessor;

        int m_width;
        int m_height;

        float m_resizeFactor;

        cv::Mat m_matDepth;
        cv::Mat m_matForeground;
        cv::Mat m_matSearched;
        cv::Mat m_matScore;
        cv::Mat m_matArea;
        cv::Mat m_updateSegmentation;
        cv::Mat m_createSegmentation;
        cv::Mat m_layerSegmentation;

        DebugVisualizer m_debugVisualizer;
    };

}}}

#endif // HANDTRACKER_H
