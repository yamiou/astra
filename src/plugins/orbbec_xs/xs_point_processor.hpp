#ifndef XS_POINT_PROCESSOR_H
#define XS_POINT_PROCESSOR_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra/astra.hpp>
#include "xs_pointstream.hpp"

namespace astra { namespace xs {

    class point_processor : public frame_listener
    {
    public:
        point_processor(PluginServiceProxy& pluginService,
                       astra_streamset_t streamset,
                       stream_description& depthDesc);
        virtual ~point_processor();
        virtual void on_frame_ready(stream_reader& reader, frame& frame) override;

    private:
        void create_point_stream_if_necessary(depthframe& depthFrame);

        void update_pointframe_from_depth(depthframe& depthFrame);
        void calculate_point_frame(depthframe& depthFrame,
                                   vector3f* p_points);

        streamset streamset_;
        astra_streamset_t setHandle_;
        stream_reader reader_;
        depthstream depthStream_;
        PluginServiceProxy& pluginService_;

        using pointstream_ptr = std::unique_ptr<pointstream>;
        pointstream_ptr pointStream_;

        conversion_cache_t depthConversionCache_;
    };
}}

#endif // XS_POINT_PROCESSOR_H
