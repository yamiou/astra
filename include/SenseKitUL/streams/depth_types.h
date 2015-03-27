#ifndef DEPTH_TYPES_H
#define DEPTH_TYPES_H

#include <SenseKit.h>

typedef struct _sensekit_depthframe {
    sensekit_frame_ref_t* frameRef;
    uint32_t frameIndex;
    uint32_t width;
    uint32_t height;
    uint8_t bpp;
    int16_t* data;
} sensekit_depthframe_t;

typedef struct _sensekit_depthstream sensekit_depthstream_t;

#endif // DEPTH_TYPES_H
