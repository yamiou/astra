#ifndef POINT_CAPI_H
#define POINT_CAPI_H

#include <SenseKit/sensekit_types.h>
#include "point_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_pointstream(sensekit_reader_t reader,
                                                                  sensekit_pointstream_t* pointStream);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_pointframe(sensekit_reader_frame_t readerFrame,
                                                                sensekit_pointframe_t* pointFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_pointframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                             sensekit_stream_subtype_t subtype,
                                                                             sensekit_pointframe_t* pointFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_data_byte_length(sensekit_pointframe_t pointFrame,
                                                                           size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_data_ptr(sensekit_pointframe_t pointFrame,
                                                                   sensekit_vector3f_t** data,
                                                                   size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_copy_data(sensekit_pointframe_t pointFrame,
                                                                sensekit_vector3f_t* data);

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_metadata(sensekit_pointframe_t pointFrame,
                                                                   sensekit_image_metadata_t* metadata);

SENSEKIT_API_EX sensekit_status_t sensekit_pointframe_get_frameindex(sensekit_pointframe_t pointFrame,
                                                                     sensekit_frame_index_t* index);
SENSEKIT_END_DECLS

#endif /* POINT_CAPI_H */
