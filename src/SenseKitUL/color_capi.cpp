#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/streams/video_parameters.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <string.h>
#include <SenseKitUL/StreamTypes.h>
#include <cassert>
#include <SenseKitUL/streams/image_capi.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get(sensekit_reader_t reader,
                                                            sensekit_colorstream_t* colorStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_COLOR,
                                      DEFAULT_SUBTYPE,
                                      colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get_hfov(sensekit_colorstream_t colorStream,
                                                                 float* hFov)
{
    return sensekit_stream_get_parameter_fixed(colorStream,
                                               STREAM_PARAMETER_HFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(hFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get_vfov(sensekit_colorstream_t colorStream,
                                                                 float* vFov)
{
    return sensekit_stream_get_parameter_fixed(colorStream,
                                               STREAM_PARAMETER_VFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(vFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get_by_type(sensekit_reader_t reader,
                                                                    sensekit_stream_type_t type,
                                                                    sensekit_stream_subtype_t subtype,
                                                                    sensekit_colorstream_t* colorStream)

{
    return sensekit_reader_get_stream(reader,
                                      type,
                                      subtype,
                                      colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_colorframe_t* colorFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_COLOR,
                                          DEFAULT_SUBTYPE,
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
    return sensekit_imageframe_get_data_byte_length(colorFrame, length);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_data_ptr(sensekit_colorframe_t colorFrame,
                                                                   uint8_t** data,
                                                                   size_t* length)
{
    void* voidData = nullptr;
    sensekit_imageframe_get_data_ptr(colorFrame, &voidData, length);
    *data = static_cast<uint8_t*>(voidData);

    return SENSEKIT_STATUS_SUCCESS;

}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_copy_data(sensekit_colorframe_t colorFrame,
                                                                uint8_t* data)
{
    return sensekit_imageframe_copy_data(colorFrame, data);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_metadata(sensekit_colorframe_t colorFrame,
                                                                   sensekit_image_metadata_t* metadata )
{
    return sensekit_imageframe_get_metadata(colorFrame, metadata);
}

SENSEKIT_END_DECLS
