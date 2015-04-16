#ifndef ONICOLORSTREAM_H
#define ONICOLORSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit { namespace plugins {

        class OniColorStream : public OniDeviceStream<sensekit_colorframe_wrapper_t,
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
                                  oniDevice)
                {
                    m_oniStream.create(m_oniDevice, ::openni::SENSOR_COLOR);
                    m_oniVideoMode = m_oniStream.getVideoMode();
                    m_bufferLength =
                        m_oniVideoMode.getResolutionX() *
                        m_oniVideoMode.getResolutionY() *
                        3;
                }

        private:
            void on_new_buffer(sensekit_frame_t* newBuffer,
                               wrapper_type* wrapper) override;
        };
    }}

#endif /* ONICOLORSTREAM_H */
