#include "OniDepthStream.h"
#include <SenseKitUL/streams/depth_parameters.h>

namespace sensekit { namespace plugins {

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

        void OniDepthStream::get_parameter(sensekit_streamconnection_t connection,
                                           sensekit_parameter_id id,
                                           sensekit_parameter_bin_t& parameterBin)
        {
            switch (id)
            {
            case DEPTH_PARAMETER_CONVERSION_CACHE:
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
                return;
            }
            }

            OniDeviceStream::get_parameter(connection, id, parameterBin);
        }
    }}
