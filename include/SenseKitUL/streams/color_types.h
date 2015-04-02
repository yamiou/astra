#ifndef COLOR_TYPES_H
#define COLOR_TYPES_H

#include <sensekit_core.h>

typedef struct _sensekit_colorframe {
    sensekit_frame_ref_t* frameRef;
    uint32_t frameIndex;
    uint32_t width;
    uint32_t height;
    uint8_t bpp;
    uint8_t* data;
} sensekit_colorframe_t;

typedef struct _sensekit_colorstream sensekit_colorstream_t;

#endif // COLOR_TYPES_H
