#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <string.h>
#include <SenseKitUL/streams/image_types.h>
#include <SenseKitUL/streams/image_capi.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_imageframe(sensekit_reader_frame_t readerFrame,
                                                                 sensekit_stream_type_t type,
                                                                 sensekit_stream_subtype_t subtype,
                                                                 sensekit_imageframe_t* imageFrame)
{
    return sensekit_generic_frame_get<sensekit_imageframe_wrapper_t>(readerFrame,
                                                                     type,
                                                                     subtype,
                                                                     imageFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_frameindex(sensekit_imageframe_t imageFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(imageFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_data_byte_length(sensekit_imageframe_t imageFrame,
                                                                           size_t* length)
{
    sensekit_image_metadata_t metadata = imageFrame->metadata;

    size_t size = metadata.width * metadata.height * metadata.bytesPerPixel;
    *length = size;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_data_ptr(sensekit_imageframe_t imageFrame,
                                                                   void** data,
                                                                   size_t* byteLength)
{
    *data = imageFrame->data;
    sensekit_imageframe_get_data_byte_length(imageFrame, byteLength);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_copy_data(sensekit_imageframe_t imageFrame,
                                                                void* data)
{
    size_t byteLength;
    sensekit_imageframe_get_data_byte_length(imageFrame, &byteLength);

    memcpy(data, imageFrame->data, byteLength);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_imageframe_get_metadata(sensekit_imageframe_t imageFrame,
                                                                   sensekit_image_metadata_t* metadata )
{
    *metadata = imageFrame->metadata;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
