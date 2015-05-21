#ifndef ONIDEPTHSTREAM_H
#define ONIDEPTHSTREAM_H

#include "OniDeviceStream.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include <cmath>

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
        {}

    private:
        virtual void on_open() override
        {
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

        conversion_cache_t m_conversionCache;
    };
}}

#endif /* ONIDEPTHSTREAM_H */
