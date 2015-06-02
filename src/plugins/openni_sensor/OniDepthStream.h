#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include <cmath>
#include <Shiny.h>

namespace sensekit { namespace plugins {

    class OniDepthStream : public OniDeviceStream<sensekit_imageframe_wrapper_t,
                                                  int16_t>
    {
    public:
        OniDepthStream(PluginServiceProxy& pluginService,
                       Sensor streamSet,
                       ::openni::Device& oniDevice)
            : OniDeviceStream(pluginService,
                              streamSet,
                              StreamDescription(
                                  SENSEKIT_STREAM_DEPTH,
                                  DEFAULT_SUBTYPE),
                              oniDevice,
                              ::openni::SENSOR_DEPTH,
                              1)
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

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) override;

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override
        {
            PROFILE_FUNC();
            
            OniDeviceStream::on_connection_removed(bin, connection);

            PROFILE_UPDATE();

            #if __ANDROID__
                PROFILE_OUTPUT("/sdcard/openni_sensor_profile.txt");
            #else
                PROFILE_OUTPUT("openni_sensor_profile.txt");
            #endif
        }

        conversion_cache_t m_conversionCache;
    };
}}

#endif /* ONIDEPTHSTREAM_H */
