#ifndef HND_HAND_TRACKER_H
#define HND_HAND_TRACKER_H

#include <opencv2/imgproc/imgproc.hpp>
#include <astra/astra.hpp>
#include <astra/capi/streams/stream_types.h>
#include <astra_core/Plugins/PluginKit.h>

#include "hnd_depth_utility.hpp"
#include "hnd_tracked_point.hpp"
#include "hnd_point_processor.hpp"
#include "hnd_scaling_coordinate_mapper.hpp"
#include "hnd_handstream.hpp"
#include "hnd_debug_handstream.hpp"
#include "hnd_debug_visualizer.hpp"
#include "hnd_settings.hpp"
#include <memory>

namespace astra { namespace hand {

    class hand_tracker : public frame_listener
    {
    public:
        hand_tracker(PluginServiceProxy& pluginService,
                     astra_streamset_t streamSet,
                     stream_description& depthDesc,
                     hand_settings& settings);

        virtual ~hand_tracker();
        virtual void on_frame_ready(stream_reader& reader, frame& frame) override;
    private:
        void create_streams(PluginServiceProxy& pluginService, astra_streamset_t streamSet);
        void reset();
        void generate_hand_frame(astra_frame_index_t frameIndex);
        static void copy_position(cv::Point3f& source, astra_vector3f_t& target);
        static astra_handstatus_t convert_hand_status(tracking_status status, tracked_point_type type);
        static void reset_hand_point(astra_handpoint_t& point);

        void overlay_circle(_astra_imageframe& imageFrame);
        void update_debug_image_frame(_astra_imageframe& astraColorframe);
        void generate_hand_debug_image_frame(astra_frame_index_t frameIndex);
        void update_tracking(depthframe& depthFrame, pointframe& pointFrame);
        void update_hand_frame(std::vector<tracked_point>& internaltracked_points, _astra_handframe& frame);

        void debug_probe_point(tracking_matrices& matrices);
        void debug_spawn_point(tracking_matrices& matrices);

        void track_points(cv::Mat& matDepth,
                          cv::Mat& matDepthFullSize,
                          cv::Mat& matForeground,
                          const vector3f* worldPoints);
        cv::Point get_mouse_probe_position();
        cv::Point get_spawn_position();

        //fields

        streamset streamset_;
        stream_reader reader_;
        depthstream depthStream_;

        hand_settings& settings_;
        PluginServiceProxy& pluginService_;
        depth_utility depthUtility_;
        point_processor pointProcessor_;

        float processingSizeWidth_;
        float processingSizeHeight_;

        using colorstream_ptr = std::unique_ptr<debug_handstream>;
        colorstream_ptr debugimagestream_;

        using handstream_ptr = std::unique_ptr<handstream>;
        handstream_ptr handStream_;

        cv::Mat matDepth_;
        cv::Mat matDepthFullSize_;
        cv::Mat matDepthWindow_;
        cv::Mat matVelocitySignal_;
        cv::Mat matArea_;
        cv::Mat matAreaSqrt_;
        cv::Mat layerIntegralArea_;
        cv::Mat debugUpdateSegmentation_;
        cv::Mat debugCreateSegmentation_;
        cv::Mat debugRefineSegmentation_;
        cv::Mat updateForegroundSearched_;
        cv::Mat createForegroundSearched_;
        cv::Mat debugUpdateScore_;
        cv::Mat debugCreateScore_;
        cv::Mat debugRefineScore_;
        cv::Mat debugUpdateScoreValue_;
        cv::Mat debugCreateScoreValue_;
        cv::Mat debugRefineScoreValue_;
        cv::Mat debugCreateTestPassMap_;
        cv::Mat debugUpdateTestPassMap_;
        cv::Mat debugRefineTestPassMap_;

        cv::Mat layerSegmentation_;
        cv::Mat layerScore_;
        cv::Mat layerEdgeDistance_;
        cv::Mat layerTestPassMap_;

        cv::Mat refineForegroundSearched_;
        cv::Mat refineSegmentation_;
        cv::Mat refineScore_;
        cv::Mat refineEdgeDistance_;

        astra::vector3f* worldPoints_{nullptr};
        int numWorldPoints_{0};

        debug_visualizer debugVisualizer_;
    };

}}

#endif // HND_HAND_TRACKER_H
