#ifndef ONI_INFRARED_STREAM_H
#define ONI_INFRARED_STREAM_H

#include "oni_devicestream.hpp"
#include <Astra/Plugins/plugin_capi.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>

namespace orbbec { namespace ni {

    class infrared_stream : public devicestream<astra_imageframe_wrapper_t>
    {
    public:
        infrared_stream(astra::PluginServiceProxy& pluginService,
                        astra_streamset_t streamSet,
                        openni::Device& oniDevice,
                        stream_listener& listener);

        infrared_stream(const infrared_stream&) = delete;
        infrared_stream& operator=(const infrared_stream&) = delete;
    };
}}

#endif /* ONI_INFRARED_STREAM_H */
