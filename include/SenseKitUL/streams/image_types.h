#ifndef IMAGE_TYPES_H
#define IMAGE_TYPES_H

#include <stdint.h>

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} sensekit_rgb888_pixel_t;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} sensekit_rgba8888_pixel_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bytesPerPixel;
} sensekit_image_metadata_t;

typedef struct _sensekit_imageframe* sensekit_imageframe_t;

#endif // IMAGE_TYPES_H
