#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <Astra/Plugins/plugin_capi.h>
#include <AstraUL/Plugins/stream_types.h>
#include <AstraUL/skul_ctypes.h>
#include <cmath>
#include <Shiny.h>

namespace astra { namespace plugins {

    class OniDepthStream : public OniDeviceStream<astra_imageframe_wrapper_t,
                                                  int16_t>
    {
    public:
        OniDepthStream(PluginServiceProxy& pluginService,
                       astra_streamset_t streamSet,
                       ::openni::Device& oniDevice)
            : OniDeviceStream(pluginService,
                              streamSet,
                              StreamDescription(
                                  ASTRA_STREAM_DEPTH,
                                  DEFAULT_SUBTYPE),
                              oniDevice,
                              ::openni::SENSOR_DEPTH)
        {
            PROFILE_FUNC();
        }

    private:
        virtual void on_open() override
        {
            PROFILE_FUNC();
            refresh_conversion_cache(m_oniStream.getHorizontalFieldOfView(),
                                     m_oniStream.getVerticalFieldOfView(),
                                     m_oniVideoMode.getResolutionX(),
                                     m_oniVideoMode.getResolutionY());
        }

        void refresh_conversion_cache(float horizontalFov,
                                      float verticalFov,
                                      int resolutionX,
                                      int resolutionY)
        {
            PROFILE_FUNC();
            m_conversionCache.xzFactor = tan(horizontalFov / 2) * 2;
            m_conversionCache.yzFactor = tan(verticalFov / 2) * 2;
            m_conversionCache.resolutionX = resolutionX;
            m_conversionCache.resolutionY = resolutionY;
            m_conversionCache.halfResX = m_conversionCache.resolutionX / 2;
            m_conversionCache.halfResY = m_conversionCache.resolutionY / 2;
            m_conversionCache.coeffX = m_conversionCache.resolutionX / m_conversionCache.xzFactor;
            m_conversionCache.coeffY = m_conversionCache.resolutionY / m_conversionCache.yzFactor;
        }

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) override;

        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override
        {
            PROFILE_FUNC();

            OniDeviceStream::on_connection_removed(bin, connection);

            #ifdef __ANDROID__
                PROFILE_UPDATE();
                PROFILE_OUTPUT("/sdcard/profile_openni_sensor.txt");
            #endif
        }

        conversion_cache_t m_conversionCache;
    };
}}

#endif /* ONIDEPTHSTREAM_H */
