#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                      float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API_EX sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ,
                                                      float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_open(sensekit_streamset_t* streamset,
                                                   sensekit_depthstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_close(sensekit_depthstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream,
                                                         int timeout, sensekit_depthframe_t** frame);

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame);

SENSEKIT_END_DECLS

#endif // DEPTH_H
