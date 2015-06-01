#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKitUL/SenseKitUL.h>
#include "PointStream.h"

namespace sensekit { namespace plugins { namespace xs {

    class PointProcessor : public FrameReadyListener
    {
    public:
        PointProcessor(PluginServiceProxy& pluginService,
                       Sensor streamset,
                       StreamDescription& depthDesc,
                       PluginLogger& pluginLogger);
        virtual ~PointProcessor();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;

    private:
        void create_point_stream_if_necessary(DepthFrame& depthFrame);

        void update_pointframe_from_depth(DepthFrame& depthFrame);
        void calculate_point_frame(DepthFrame& depthFrame,
                                   Vector3f* p_points);

        PluginServiceProxy& m_pluginService;
        Sensor m_streamset;
        PluginLogger& m_logger;
        StreamReader m_reader;
        DepthStream m_depthStream;

        using PointStreamPtr = std::unique_ptr<PointStream>;
        PointStreamPtr m_pointStream;

        conversion_cache_t m_depthConversionCache;
    };

} } }

#endif // POINTPROCESSOR_H
