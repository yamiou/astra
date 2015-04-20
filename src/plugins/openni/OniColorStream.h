#ifndef ONICOLORSTREAM_H
#define ONICOLORSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit { namespace plugins {

        class OniColorStream : public OniDeviceStream<sensekit_imageframe_wrapper_t,
                                                      uint8_t>
        {
        public:
            OniColorStream(PluginServiceProxy& pluginService,
                           Sensor& streamSet,
                           ::openni::Device& oniDevice)
                : OniDeviceStream(pluginService,
                                  streamSet,
                                  StreamDescription(
                                      SENSEKIT_STREAM_COLOR,
                                      DEFAULT_SUBTYPE),
                                  oniDevice,
                                  ::openni::SENSOR_COLOR,
                                  3)
                { }

        private:

        };
    }}

#endif /* ONICOLORSTREAM_H */
