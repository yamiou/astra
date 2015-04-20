#ifndef DEPTH_CAPI_H
#define DEPTH_CAPI_H

#include <SenseKit/sensekit_defines.h>
#include <SenseKit/sensekit_types.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_convert_depth_to_world(sensekit_depthstream_t depthStream,
                                                                  float depthX, float depthY, float depthZ,
                                                                  float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API_EX sensekit_status_t sensekit_convert_world_to_depth(sensekit_depthstream_t depthStream,
                                                                  float worldX, float worldY, float worldZ,
                                                                  float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_depthstream(sensekit_reader_t reader,
                                                                  sensekit_depthstream_t* depthStream);

SENSEKIT_API_EX sensekit_status_t sensekit_depthstream_get_hfov(sensekit_depthstream_t depthStream,
                                                                float* hFov);

SENSEKIT_API_EX sensekit_status_t sensekit_depthstream_get_vfov(sensekit_depthstream_t depthStream,
                                                                float* vFov);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_depthframe(sensekit_reader_frame_t readerFrame,
                                                                sensekit_depthframe_t* depthFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_depthframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                             sensekit_stream_subtype_t subtype,
                                                                             sensekit_depthframe_t* colorFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_byte_length(sensekit_depthframe_t depthFrame,
                                                                           size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_ptr(sensekit_depthframe_t depthFrame,
                                                                   int16_t** data,
                                                                   size_t* byteLength);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_copy_data(sensekit_depthframe_t depthFrame,
                                                                int16_t* data);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_metadata(sensekit_depthframe_t depthFrame,
                                                                   sensekit_image_metadata_t* metadata );

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_frameindex(sensekit_depthframe_t depthFrame,
                                                                     sensekit_frame_index_t* index);

SENSEKIT_END_DECLS

#endif // DEPTH_CAPI_H
