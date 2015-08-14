#include <SenseKit/sensekit_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <string.h>
#include <cassert>
#include <SenseKitUL/streams/image_capi.h>
#include <SenseKitUL/streams/image_parameters.h>
#include <stdbool.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_colorstream(sensekit_reader_t reader,
                                                                  sensekit_colorstream_t* colorStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_COLOR,
                                      DEFAULT_SUBTYPE,
                                      colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_colorframe(sensekit_reader_frame_t readerFrame,
                                                                sensekit_colorframe_t* colorFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_COLOR,
                                          DEFAULT_SUBTYPE,
                                          colorFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_colorframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                             sensekit_stream_subtype_t subtype,
                                                                             sensekit_colorframe_t* colorFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_COLOR,
                                          subtype,
                                          colorFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_frameindex(sensekit_colorframe_t colorFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(colorFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_data_byte_length(sensekit_colorframe_t colorFrame,
                                                                           size_t* byteLength)
{
    return sensekit_imageframe_get_data_byte_length(colorFrame, byteLength);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_data_ptr(sensekit_colorframe_t colorFrame,
                                                                   uint8_t** data,
                                                                   size_t* byteLength)
{
    void* voidData = nullptr;
    sensekit_imageframe_get_data_ptr(colorFrame, &voidData, byteLength);
    *data = static_cast<uint8_t*>(voidData);

    return SENSEKIT_STATUS_SUCCESS;

}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_copy_data(sensekit_colorframe_t colorFrame,
                                                                uint8_t* data)
{
    return sensekit_imageframe_copy_data(colorFrame, data);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_metadata(sensekit_colorframe_t colorFrame,
                                                                   sensekit_image_metadata_t* metadata)
{
    return sensekit_imageframe_get_metadata(colorFrame, metadata);
}

SENSEKIT_END_DECLS
