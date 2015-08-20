#ifndef HANDTRACKER_H
#define HANDTRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <AstraUL/AstraUL.h>
#include <AstraUL/Plugins/stream_types.h>
#include <Astra/Plugins/PluginKit.h>

#include "DepthUtility.h"
#include "TrackedPoint.h"
#include "PointProcessor.h"
#include "ScalingCoordinateMapper.h"
#include "HandStream.h"
#include "DebugHandStream.h"
#include "DebugVisualizer.h"
#include "HandSettings.h"
#include <memory>

namespace astra { namespace plugins { namespace hand {

    class HandTracker : public FrameReadyListener
    {
    public:
        HandTracker(PluginServiceProxy& pluginService,
                    astra_streamset_t streamSet,
                    StreamDescription& depthDesc,
                    HandSettings& settings);

        virtual ~HandTracker();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;
    private:
        void create_streams(PluginServiceProxy& pluginService, astra_streamset_t streamSet);
        void reset();
        void generate_hand_frame(astra_frame_index_t frameIndex);
        static void copy_position(cv::Point3f& source, astra_vector3f_t& target);
        static astra_handstatus_t convert_hand_status(TrackingStatus status, TrackedPointType type);
        static void reset_hand_point(astra_handpoint_t& point);

        void overlay_circle(_astra_imageframe& imageFrame);
        void update_debug_image_frame(_astra_imageframe& astraColorframe);
        void generate_hand_debug_image_frame(astra_frame_index_t frameIndex);
        void update_tracking(DepthFrame& depthFrame, PointFrame& pointFrame);
        void update_hand_frame(std::vector<TrackedPoint>& internalTrackedPoints, _astra_handframe& frame);

        void debug_probe_point(TrackingMatrices& matrices);
        void debug_spawn_point(TrackingMatrices& matrices);

        void track_points(cv::Mat& matDepth,
                          cv::Mat& matDepthFullSize,
                          cv::Mat& matForeground,
                          const Vector3f* worldPoints);
        cv::Point get_mouse_probe_position();
        cv::Point get_spawn_position();

        //fields

        Sensor m_sensor;
        StreamReader m_reader;
        DepthStream m_depthStream;

        HandSettings& m_settings;
        PluginServiceProxy& m_pluginService;
        DepthUtility m_depthUtility;
        PointProcessor m_pointProcessor;

        float m_processingSizeWidth;
        float m_processingSizeHeight;

        using ColorStreamPtr = std::unique_ptr<DebugHandStream>;
        ColorStreamPtr m_debugImageStream;

        using HandStreamPtr = std::unique_ptr<HandStream>;
        HandStreamPtr m_handStream;

        cv::Mat m_matDepth;
        cv::Mat m_matDepthFullSize;
        cv::Mat m_matDepthWindow;
        cv::Mat m_matVelocitySignal;
        cv::Mat m_matArea;
        cv::Mat m_matAreaSqrt;
        cv::Mat m_layerIntegralArea;
        cv::Mat m_debugUpdateSegmentation;
        cv::Mat m_debugCreateSegmentation;
        cv::Mat m_debugRefineSegmentation;
        cv::Mat m_updateForegroundSearched;
        cv::Mat m_createForegroundSearched;
        cv::Mat m_debugUpdateScore;
        cv::Mat m_debugCreateScore;
        cv::Mat m_debugRefineScore;
        cv::Mat m_debugUpdateScoreValue;
        cv::Mat m_debugCreateScoreValue;
        cv::Mat m_debugRefineScoreValue;
        cv::Mat m_debugCreateTestPassMap;
        cv::Mat m_debugUpdateTestPassMap;
        cv::Mat m_debugRefineTestPassMap;

        cv::Mat m_layerSegmentation;
        cv::Mat m_layerScore;
        cv::Mat m_layerEdgeDistance;
        cv::Mat m_layerTestPassMap;

        cv::Mat m_refineForegroundSearched;
        cv::Mat m_refineSegmentation;
        cv::Mat m_refineScore;
        cv::Mat m_refineEdgeDistance;

        astra::Vector3f* m_worldPoints { nullptr };
        int m_numWorldPoints { 0 };

        DebugVisualizer m_debugVisualizer;
    };

}}}

#endif // HANDTRACKER_H
