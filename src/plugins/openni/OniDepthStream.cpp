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

        void OniDepthStream::get_parameter_size(sensekit_streamconnection_t connection,
                                                sensekit_parameter_id id,
                                                size_t& byteLength)
        {
            switch (id)
            {
            case DEPTH_PARAMETER_CONVERSION_CACHE:
            {
                byteLength = sizeof(conversion_cache_t);
                return;
            }
            }

            OniDeviceStream::get_parameter_size(connection, id, byteLength);
        }

        void OniDepthStream::get_parameter_data(sensekit_streamconnection_t connection,
                                                sensekit_parameter_id id,
                                                size_t byteLength,
                                                sensekit_parameter_data_t* data)
        {
            switch (id)
            {
            case DEPTH_PARAMETER_CONVERSION_CACHE:
            {
                assert(byteLength >= sizeof(conversion_cache_t));
                conversion_cache_t* cache = reinterpret_cast<conversion_cache_t*>(data);
                *cache = m_conversionCache;
                return;
            }
            }

            OniDeviceStream::get_parameter_data(connection, id, byteLength, data);

        }
    }}
