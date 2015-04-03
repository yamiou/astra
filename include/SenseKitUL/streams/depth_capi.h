#ifndef DEPTH_CAPI_H
#define DEPTH_CAPI_H

#include <sensekit_core.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                         float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API_EX sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ,
                                                         float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_stream_get(sensekit_reader_t* reader,
                                                           sensekit_depthstream_t** depthStream);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_get(sensekit_reader_frame_t* readerFrame,
                                                           sensekit_depthframe_t** depthFrame);

SENSEKIT_END_DECLS

#endif // DEPTH_CAPI_H
