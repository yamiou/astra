#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/StreamTypes.h>
#include <cmath>

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

                    refresh_conversion_cache(m_oniStream.getHorizontalFieldOfView(),
                                             m_oniStream.getVerticalFieldOfView(),
                                             m_oniVideoMode.getResolutionX(),
                                             m_oniVideoMode.getResolutionY());
                }

        private:
            virtual void on_new_buffer(sensekit_frame_t* newBuffer,
                                       wrapper_type* wrapper) override;

            void refresh_conversion_cache(float horizontalFov,
                                          float verticalFov,
                                          int resolutionX,
                                          int resolutionY)
                {
                    m_conversionCache.xzFactor = tan(horizontalFov / 2) * 2;
                    m_conversionCache.yzFactor = tan(verticalFov / 2) * 2;
                    m_conversionCache.resolutionX = resolutionX;
                    m_conversionCache.resolutionY = resolutionY;
                    m_conversionCache.halfResX = m_conversionCache.resolutionX / 2;
                    m_conversionCache.halfResY = m_conversionCache.resolutionY / 2;
                    m_conversionCache.coeffX = m_conversionCache.resolutionX / m_conversionCache.xzFactor;
                    m_conversionCache.coeffY = m_conversionCache.resolutionY / m_conversionCache.yzFactor;
                }

            virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                            sensekit_parameter_id id,
                                            size_t& byteLength) override;

            virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                            sensekit_parameter_id id,
                                            size_t byteLength,
                                            sensekit_parameter_data_t* data) override;

            conversion_cache_t m_conversionCache;
        };
    }}

#endif /* ONIDEPTHSTREAM_H */
