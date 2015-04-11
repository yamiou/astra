#ifndef DEPTH_TYPES_H
#define DEPTH_TYPES_H

#include <SenseKit/sensekit_types.h>

typedef struct
{
    float xzFactor;
    float yzFactor;
    float coeffX;
    float coeffY;
    int resolutionX;
    int resolutionY;
    int halfResX;
    int halfResY;
} conversion_cache_t;

typedef sensekit_streamconnection_t sensekit_depthstream_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bytesPerPixel;
} sensekit_depthframe_metadata_t;

typedef struct _sensekit_depthframe* sensekit_depthframe_t;

#endif // DEPTH_TYPES_H
