#include "OniDepthStream.h"
#include <SenseKitUL/streams/depth_parameters.h>

namespace sensekit { namespace plugins {

    void OniDepthStream::on_get_parameter(sensekit_streamconnection_t connection,
                                          sensekit_parameter_id id,
                                          sensekit_parameter_bin_t& parameterBin)
    {
        PROFILE_FUNC();
        switch (id)
        {
        case SENSEKIT_PARAMETER_DEPTH_CONVERSION_CACHE:
        {
            size_t resultByteLength = sizeof(conversion_cache_t);

            sensekit_parameter_data_t parameterData;
            sensekit_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                         &parameterBin,
                                                                         &parameterData);
            if (rc == SENSEKIT_STATUS_SUCCESS)
            {
                memcpy(parameterData, &m_conversionCache, resultByteLength);
            }
            break;
        }
        default:
            OniDeviceStream::on_get_parameter(connection, id, parameterBin);
            break;
        }
    }
}}
