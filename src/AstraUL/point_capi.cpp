#include <Astra/astra_types.h>
#include <AstraUL/astraul_ctypes.h>
#include "generic_stream_api.h"
#include <AstraUL/streams/point_capi.h>
#include <AstraUL/streams/point_types.h>
#include <AstraUL/Plugins/stream_types.h>
#include <string.h>
#include <cassert>
#include <AstraUL/streams/image_capi.h>

ASTRA_BEGIN_DECLS

ASTRA_API_EX astra_status_t astra_reader_get_pointstream(astra_reader_t reader,
                                                                  astra_pointstream_t* pointStream)

{
    return astra_reader_get_stream(reader,
                                      ASTRA_STREAM_COLOR,
                                      DEFAULT_SUBTYPE,
                                      pointStream);
}

ASTRA_API_EX astra_status_t astra_frame_get_pointframe(astra_reader_frame_t readerFrame,
                                                                astra_pointframe_t* pointFrame)
{
    return astra_reader_get_imageframe(readerFrame,
                                          ASTRA_STREAM_POINT,
                                          DEFAULT_SUBTYPE,
                                          pointFrame);
}

ASTRA_API_EX astra_status_t astra_frame_get_pointframe_with_subtype(astra_reader_frame_t readerFrame,
                                                                             astra_stream_subtype_t subtype,
                                                                             astra_pointframe_t* pointFrame)
{
    return astra_reader_get_imageframe(readerFrame,
                                          ASTRA_STREAM_POINT,
                                          subtype,
                                          pointFrame);
}

ASTRA_API_EX astra_status_t astra_pointframe_get_frameindex(astra_pointframe_t pointFrame,
                                                                     astra_frame_index_t* index)
{
    return astra_generic_frame_get_frameindex(pointFrame, index);
}

ASTRA_API_EX astra_status_t astra_pointframe_get_data_byte_length(astra_pointframe_t pointFrame,
                                                                           size_t* byteLength)
{
    return astra_imageframe_get_data_byte_length(pointFrame, byteLength);
}

ASTRA_API_EX astra_status_t astra_pointframe_get_data_ptr(astra_pointframe_t pointFrame,
                                                                   astra_vector3f_t** data,
                                                                   size_t* byteLength)
{
    void* voidData = nullptr;
    astra_imageframe_get_data_ptr(pointFrame, &voidData, byteLength);
    *data = static_cast<astra_vector3f_t*>(voidData);

    return ASTRA_STATUS_SUCCESS;
}

ASTRA_API_EX astra_status_t astra_pointframe_copy_data(astra_pointframe_t pointFrame,
                                                                astra_vector3f_t* data)
{
    return astra_imageframe_copy_data(pointFrame, data);
}

ASTRA_API_EX astra_status_t astra_pointframe_get_metadata(astra_pointframe_t pointFrame,
                                                                   astra_image_metadata_t* metadata)
{
    return astra_imageframe_get_metadata(pointFrame, metadata);
}

ASTRA_END_DECLS
