#ifndef ONICOLORSTREAM_H
#define ONICOLORSTREAM_H

#include "OniDeviceStream.h"
#include <Astra/Plugins/plugin_capi.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <Shiny.h>

namespace astra { namespace plugins {

    class OniColorStream : public OniDeviceStream<astra_imageframe_wrapper_t,
                                                  uint8_t>
    {
    public:
        OniColorStream(PluginServiceProxy& pluginService,
                       astra_streamset_t streamSet,
                       ::openni::Device& oniDevice)
            : OniDeviceStream(pluginService,
                              streamSet,
                              StreamDescription(
                                  ASTRA_STREAM_COLOR,
                                  DEFAULT_SUBTYPE),
                              oniDevice,
                              ::openni::SENSOR_COLOR)
        {
            PROFILE_FUNC();
        }
    };
}}

#endif /* ONICOLORSTREAM_H */
