#ifndef IMAGE_TYPES_H
#define IMAGE_TYPES_H

#include <stdint.h>

#if defined(__GNUC__) || defined(__clang__)
#define PACK_STRUCT __attribute__((packed))
#else
#define PACK_STRUCT
#endif

#ifdef _MSC_VER
#pragma pack(push, 1)
#endif


typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} PACK_STRUCT sensekit_rgb_pixel_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bytesPerPixel;
} PACK_STRUCT sensekit_image_metadata_t;

#ifdef _MSC_VER
#pragma pack(pop)
#endif

typedef struct _sensekit_imageframe* sensekit_imageframe_t;

#endif // IMAGE_TYPES_H
