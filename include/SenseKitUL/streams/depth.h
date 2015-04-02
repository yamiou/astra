#ifndef DEPTH_H
#define DEPTH_H

#include <sensekit_core.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                         float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API_EX sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ,
                                                         float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_get(sensekit_reader_t* reader,
                                                     sensekit_depthstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream,
                                                            int timeoutMillis,
                                                            sensekit_depthframe_t** frame);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame);

SENSEKIT_END_DECLS

#endif // DEPTH_H
