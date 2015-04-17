#ifndef IMAGE_CAPI_H
#define IMAGE_CAPI_H

#include <SenseKit/sensekit_defines.h>
#include <SenseKit/sensekit_types.h>
#include "image_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_image_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_stream_type_t type,
                                                           sensekit_stream_subtype_t subType,
                                                           sensekit_imageframe_t* imageFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_frameindex(sensekit_imageframe_t imageFrame,
                                                                     sensekit_frame_index_t* index);

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_data_byte_length(sensekit_imageframe_t imageFrame,
                                                                           size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_data_ptr(sensekit_imageframe_t imageFrame,
                                                                   void** data,
                                                                   size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_copy_data(sensekit_imageframe_t imageFrame,
                                                                void* data);

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_metadata(sensekit_imageframe_t imageFrame,
                                                                   sensekit_image_metadata_t* metadata );

SENSEKIT_END_DECLS

#endif // IMAGE_CAPI_H
