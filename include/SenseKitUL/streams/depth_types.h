#ifndef DEPTH_TYPES_H
#define DEPTH_TYPES_H

#include <sensekit_core.h>

struct conversion_cache_t
{
    float xzFactor;
    float yzFactor;
    float coeffX;
    float coeffY;
    int resolutionX;
    int resolutionY;
    int halfResX;
    int halfResY;
};

typedef sensekit_streamconnection_t sensekit_depthstream_t;

struct sensekit_depthframe_metadata_t {
    uint32_t width;
    uint32_t height;
    uint8_t bytesPerPixel;
};

typedef struct _sensekit_depthframe* sensekit_depthframe_t;

#endif // DEPTH_TYPES_H
