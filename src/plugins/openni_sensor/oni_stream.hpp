#ifndef ONI_STREAM_H
#define ONI_STREAM_H

#include <Astra/Astra.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/Stream.h>
#include <Astra/Plugins/StreamBin.h>
#include <OpenNI.h>

namespace orbbec { namespace ni {

    class stream : public astra::plugins::Stream
    {
    public:
        stream(astra::PluginServiceProxy& pluginService,
                   astra_streamset_t streamSet,
                   astra::StreamDescription desc)
            : Stream(pluginService,
                     streamSet,
                     desc)
        {
            PROFILE_FUNC();
        }

        virtual astra_status_t read_frame() = 0;
        virtual astra_status_t open() = 0;
        virtual astra_status_t close() = 0;
        virtual openni::VideoStream* get_stream() = 0;
        virtual astra_status_t start() = 0;
        virtual astra_status_t stop() = 0;
    };
}}

#endif /* ONI_STREAM_H */
