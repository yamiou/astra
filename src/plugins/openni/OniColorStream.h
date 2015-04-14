#ifndef ONICOLORSTREAM_H
#define ONICOLORSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_api.h>
#include <SenseKitUL/StreamTypes.h>

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
                    m_bufferLength = m_oniVideoMode.getResolutionX() *
                        m_oniVideoMode.getResolutionX() *
                        3;
                }

        private:
            void on_new_buffer(sensekit_frame_t* newBuffer,
                               wrapper_type* wrapper) override;
        };

        void OniColorStream::on_new_buffer(sensekit_frame_t* newBuffer,
                                           wrapper_type* wrapper)
        {
            if (wrapper == nullptr)
                return;

            sensekit_colorframe_metadata_t metadata;

            metadata.width = m_oniVideoMode.getResolutionX();
            metadata.height = m_oniVideoMode.getResolutionY();
            metadata.bytesPerPixel = 3;

            wrapper->frame.metadata = metadata;
        }

    }}

#endif /* ONICOLORSTREAM_H */
