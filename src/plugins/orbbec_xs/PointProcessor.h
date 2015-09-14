#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include "PointStream.h"

namespace astra { namespace plugins { namespace xs {

    class PointProcessor : public FrameReadyListener
    {
    public:
        PointProcessor(PluginServiceProxy& pluginService,
                       astra_streamset_t streamset,
                       StreamDescription& depthDesc);
        virtual ~PointProcessor();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;

    private:
        void create_point_stream_if_necessary(DepthFrame& depthFrame);

        void update_pointframe_from_depth(DepthFrame& depthFrame);
        void calculate_point_frame(DepthFrame& depthFrame,
                                   Vector3f* p_points);

        StreamSet m_streamset;
        astra_streamset_t m_streamSet;
        StreamReader m_reader;
        DepthStream m_depthStream;
        PluginServiceProxy& m_pluginService;

        using PointStreamPtr = std::unique_ptr<PointStream>;
        PointStreamPtr m_pointStream;

        conversion_cache_t m_depthConversionCache;
    };

} } }

#endif // POINTPROCESSOR_H
