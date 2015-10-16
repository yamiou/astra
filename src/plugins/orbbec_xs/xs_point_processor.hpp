#ifndef XS_POINT_PROCESSOR_H
#define XS_POINT_PROCESSOR_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include "xs_pointstream.hpp"

namespace astra { namespace xs {

    class point_processor : public FrameReadyListener
    {
    public:
        point_processor(PluginServiceProxy& pluginService,
                       astra_streamset_t streamset,
                       StreamDescription& depthDesc);
        virtual ~point_processor();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;

    private:
        void create_point_stream_if_necessary(DepthFrame& depthFrame);

        void update_pointframe_from_depth(DepthFrame& depthFrame);
        void calculate_point_frame(DepthFrame& depthFrame,
                                   Vector3f* p_points);

        StreamSet streamset_;
        astra_streamset_t setHandle_;
        StreamReader reader_;
        DepthStream depthStream_;
        PluginServiceProxy& pluginService_;

        using pointstream_ptr = std::unique_ptr<pointstream>;
        pointstream_ptr pointStream_;

        conversion_cache_t depthConversionCache_;
    };
}}

#endif // XS_POINT_PROCESSOR_H
