#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include "depth_types.h"
#include <math.h>

static WorldConversionCache g_convertCache;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ, float* pWorldX, float* pWorldY, float* pWorldZ);

SENSEKIT_API sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ, float* pDepthX, float* pDepthY, float* pDepthZ);

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_streamset_t* sensor, /*out*/sensekit_depthstream_t** stream);

SENSEKIT_API sensekit_status_t sensekit_depth_close(/*inout*/sensekit_depthstream_t** stream);

SENSEKIT_API sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t** frame);

SENSEKIT_API sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame);

SENSEKIT_END_DECLS

#endif // DEPTH_H
