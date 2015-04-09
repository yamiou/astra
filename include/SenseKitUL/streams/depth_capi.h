#ifndef DEPTH_CAPI_H
#define DEPTH_CAPI_H

#include <sensekit_types.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                         float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API_EX sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ,
                                                         float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_stream_get(sensekit_reader_t reader,
                                                            sensekit_depthstream_t* depthStream);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_depthframe_t* depthFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_length(sensekit_depthframe_t depthFrame,
                                                                      size_t* length);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_ptr(sensekit_depthframe_t depthFrame,
                                                                   int16_t** data,
                                                                   size_t* length);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_copy_data(sensekit_depthframe_t depthFrame,
                                                                int16_t* data);

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_metadata(sensekit_depthframe_t depthFrame,
                                                                   sensekit_depthframe_metadata_t* metadata );

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_frameindex(sensekit_depthframe_t depthFrame,
                                                                     sensekit_frame_index_t* index);

SENSEKIT_END_DECLS

#endif // DEPTH_CAPI_H
