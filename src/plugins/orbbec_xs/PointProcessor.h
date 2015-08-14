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
                       sensekit_streamset_t streamset,
                       StreamDescription& depthDesc);
        virtual ~PointProcessor();
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) override;

    private:
        void create_point_stream_if_necessary(DepthFrame& depthFrame);

        void update_pointframe_from_depth(DepthFrame& depthFrame);
        void calculate_point_frame(DepthFrame& depthFrame,
                                   Vector3f* p_points);

        Sensor m_sensor;
        sensekit_streamset_t m_streamSet;
        StreamReader m_reader;
        DepthStream m_depthStream;
        PluginServiceProxy& m_pluginService;

        using PointStreamPtr = std::unique_ptr<PointStream>;
        PointStreamPtr m_pointStream;

        conversion_cache_t m_depthConversionCache;
    };

} } }

#endif // POINTPROCESSOR_H
