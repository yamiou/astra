#include "OniColorStream.h"

namespace sensekit { namespace plugins {

        void OniColorStream::on_new_buffer(sensekit_frame_t* newBuffer,
                                           wrapper_type* wrapper)
        {
            if (wrapper == nullptr)
                return;

            sensekit_image_metadata_t metadata;

            metadata.width = m_oniVideoMode.getResolutionX();
            metadata.height = m_oniVideoMode.getResolutionY();
            metadata.bytesPerPixel = 3;

            wrapper->frame.metadata = metadata;
        }

    }}
