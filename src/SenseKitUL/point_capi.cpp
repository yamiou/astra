#include <SenseKit/sensekit_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/point_capi.h>
#include <SenseKitUL/streams/point_types.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <string.h>
#include <cassert>
#include <SenseKitUL/streams/image_capi.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_pointstream(sensekit_reader_t reader,
                                                                  sensekit_pointstream_t* pointStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_COLOR,
                                      DEFAULT_SUBTYPE,
                                      pointStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_pointframe(sensekit_reader_frame_t readerFrame,
                                                                sensekit_pointframe_t* pointFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_POINT,
                                          DEFAULT_SUBTYPE,
                                          pointFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_pointframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                             sensekit_stream_subtype_t subtype,
                                                                             sensekit_pointframe_t* pointFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_POINT,
                                          subtype,
                                          pointFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_frameindex(sensekit_pointframe_t pointFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(pointFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_data_byte_length(sensekit_pointframe_t pointFrame,
                                                                           size_t* byteLength)
{
    return sensekit_imageframe_get_data_byte_length(pointFrame, byteLength);
}

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_data_ptr(sensekit_pointframe_t pointFrame,
                                                                   sensekit_vector3f_t** data,
                                                                   size_t* byteLength)
{
    void* voidData = nullptr;
    sensekit_imageframe_get_data_ptr(pointFrame, &voidData, byteLength);
    *data = static_cast<sensekit_vector3f_t*>(voidData);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_copy_data(sensekit_pointframe_t pointFrame,
                                                                sensekit_vector3f_t* data)
{
    return sensekit_imageframe_copy_data(pointFrame, data);
}

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_metadata(sensekit_pointframe_t pointFrame,
                                                                   sensekit_image_metadata_t* metadata)
{
    return sensekit_imageframe_get_metadata(pointFrame, metadata);
}

SENSEKIT_END_DECLS
