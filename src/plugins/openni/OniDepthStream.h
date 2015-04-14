#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_api.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/StreamTypes.h>

namespace sensekit { namespace plugins {

        class OniDepthStream : public OniDeviceStream<sensekit_depthframe_wrapper_t,
                                                      int16_t>
        {
        public:
            OniDepthStream(PluginServiceProxy& pluginService,
                           Sensor& streamSet,
                           ::openni::Device& oniDevice)
                : OniDeviceStream(pluginService,
                                  streamSet,
                                  StreamDescription(
                                      SENSEKIT_STREAM_DEPTH,
                                      DEFAULT_SUBTYPE),
                                  oniDevice)
                {
                    m_oniStream.create(m_oniDevice, ::openni::SENSOR_DEPTH);
                    m_oniVideoMode = m_oniStream.getVideoMode();
                    m_bufferLength =
                        m_oniVideoMode.getResolutionX() *
                        m_oniVideoMode.getResolutionY() *
                        2;
                }

        private:
            virtual void on_new_buffer(sensekit_frame_t* newBuffer,
                                       wrapper_type* wrapper) override;
        };
    }}

#endif /* ONIDEPTHSTREAM_H */
