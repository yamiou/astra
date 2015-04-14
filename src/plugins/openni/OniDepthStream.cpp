#include "OniDepthStream.h"

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
 }}
