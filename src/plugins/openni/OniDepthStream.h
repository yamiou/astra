#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_api.h>
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
                }

        private:
            virtual void on_new_buffer(sensekit_frame_t* newBuffer,
                                       wrapper_type* wrapper) override;
        };

        void OniDepthStream::on_new_buffer(sensekit_frame_t* newBuffer,
                                           wrapper_type* wrapper)
        {
            if (wrapper == nullptr)
                return;

            sensekit_depthframe_metadata_t metadata;

            metadata.width = m_oniVideoMode.getResolutionX();
            metadata.height = m_oniVideoMode.getResolutionY();
            metadata.bytesPerPixel = 2;

            wrapper->frame.metadata = metadata;
        }

        class OniColorStream : public OniDeviceStream<sensekit_colorframe_wrapper_t,
                                                      uint8_t>
        {
        public:
            OniColorStream(PluginServiceProxy& pluginService,
                           Sensor& streamSet,
                           ::openni::Device& oniDevice)
                : OniDeviceStream(pluginService, streamSet,
                                  StreamDescription(
                                      SENSEKIT_STREAM_COLOR,
                                      DEFAULT_SUBTYPE),
                                  oniDevice)
                {
                    m_oniStream.create(m_oniDevice, ::openni::SENSOR_COLOR);
                    m_oniVideoMode = m_oniStream.getVideoMode();
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

#endif /* ONIDEPTHSTREAM_H */
