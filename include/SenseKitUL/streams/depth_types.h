#ifndef DEPTH_TYPES_H
#define DEPTH_TYPES_H

#include <SenseKit/sensekit_types.h>
#include "image_types.h"

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

typedef struct _sensekit_imageframe* sensekit_depthframe_t;

#endif // DEPTH_TYPES_H
