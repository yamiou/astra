#include <sensekit_types.h>
#include "generic_stream_api.h"
#include <streams/color_types.h>
#include "SenseKitUL_internal.h"
#include <math.h>
#include <memory.h>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get(sensekit_reader_t reader,
                                                            sensekit_colorstream_t* colorStream)

{
    return sensekit_generic_stream_get(reader,
                                       SENSEKIT_STREAM_COLOR,
                                       ANY_SUBTYPE,
                                       colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_colorframe_t* colorFrame)
{
    return sensekit_generic_frame_get<sensekit_colorframe_wrapper_t>(readerFrame,
                                                                     SENSEKIT_STREAM_COLOR,
                                                                     ANY_SUBTYPE,
                                                                     colorFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_frameindex(sensekit_colorframe_t colorFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(colorFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_data_length(sensekit_colorframe_t colorFrame,
                                                                      size_t* length)
{
    sensekit_colorframe_metadata_t metadata = colorFrame->metadata;

    size_t size = metadata.width * metadata.height * metadata.bytesPerPixel;
    *length = size;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_data_ptr(sensekit_colorframe_t colorFrame,
                                                                   uint8_t** data,
                                                                   size_t* length)
{
    *data = colorFrame->data;
    sensekit_colorframe_get_data_length(colorFrame, length);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_copy_data(sensekit_colorframe_t colorFrame,
                                                                uint8_t* data)
{
    sensekit_colorframe_metadata_t metadata = colorFrame->metadata;
    size_t size = metadata.width * metadata.height * metadata.bytesPerPixel;

    memcpy(data, colorFrame->data, size);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_metadata(sensekit_colorframe_t colorFrame,
                                                                   sensekit_colorframe_metadata_t* metadata ){
    *metadata = colorFrame->metadata;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
