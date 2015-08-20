#include "OniDepthStream.h"
#include <AstraUL/streams/depth_parameters.h>

namespace astra { namespace plugins {

    void OniDepthStream::on_get_parameter(astra_streamconnection_t connection,
                                          astra_parameter_id id,
                                          astra_parameter_bin_t& parameterBin)
    {
        PROFILE_FUNC();
        switch (id)
        {
        case ASTRA_PARAMETER_DEPTH_CONVERSION_CACHE:
        {
            size_t resultByteLength = sizeof(conversion_cache_t);

            astra_parameter_data_t parameterData;
            astra_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                         &parameterBin,
                                                                         &parameterData);
            if (rc == ASTRA_STATUS_SUCCESS)
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
