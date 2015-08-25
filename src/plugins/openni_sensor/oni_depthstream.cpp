#include "oni_depthstream.hpp"
#include <AstraUL/streams/depth_parameters.h>

namespace orbbec { namespace ni {

    depthstream::depthstream(astra::PluginServiceProxy& pluginService,
                             astra_streamset_t streamSet,
                             openni::Device& oniDevice)
        : devicestream(pluginService,
                       streamSet,
                       astra::StreamDescription(
                           ASTRA_STREAM_DEPTH,
                           DEFAULT_SUBTYPE),
                       oniDevice,
                       openni::SENSOR_DEPTH)
    {
        PROFILE_FUNC();
    }

    void depthstream::on_open()
    {
        PROFILE_FUNC();
        refresh_conversion_cache(oniStream_.getHorizontalFieldOfView(),
                                 oniStream_.getVerticalFieldOfView(),
                                 oniVideoMode_.getResolutionX(),
                                 oniVideoMode_.getResolutionY());
    }

    void depthstream::refresh_conversion_cache(float horizontalFov,
                                               float verticalFov,
                                               int resolutionX,
                                               int resolutionY)
    {
        PROFILE_FUNC();
        conversionCache_.xzFactor = std::tan(horizontalFov / 2) * 2;
        conversionCache_.yzFactor = std::tan(verticalFov / 2) * 2;
        conversionCache_.resolutionX = resolutionX;
        conversionCache_.resolutionY = resolutionY;
        conversionCache_.halfResX = conversionCache_.resolutionX / 2;
        conversionCache_.halfResY = conversionCache_.resolutionY / 2;
        conversionCache_.coeffX = conversionCache_.resolutionX / conversionCache_.xzFactor;
        conversionCache_.coeffY = conversionCache_.resolutionY / conversionCache_.yzFactor;
    }


    void depthstream::on_connection_removed(astra_bin_t bin,
                                            astra_streamconnection_t connection)
    {
        PROFILE_FUNC();

        devicestream::on_connection_removed(bin, connection);

#ifdef __ANDROID__
        PROFILE_UPDATE();
        PROFILE_OUTPUT("/sdcard/profile_openni_sensor.txt");
#endif
    }

    void depthstream::on_get_parameter(astra_streamconnection_t connection,
                                       astra_parameter_id id,
                                       astra_parameter_bin_t& parameterBin)
    {
        PROFILE_FUNC();
        switch (id)
        {
        case ASTRA_PARAMETER_DEPTH_CONVERSION_CACHE:
        {
            std::size_t resultByteLength = sizeof(conversion_cache_t);

            astra_parameter_data_t parameterData;
            astra_status_t rc = pluginService().get_parameter_bin(resultByteLength,
                                                                      &parameterBin,
                                                                      &parameterData);
            if (rc == ASTRA_STATUS_SUCCESS)
            {
                std::memcpy(parameterData, &conversionCache_, resultByteLength);
            }
            break;
        }
        default:
            devicestream::on_get_parameter(connection, id, parameterBin);
            break;
        }
    }
}}
