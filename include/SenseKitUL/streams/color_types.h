#ifndef COLOR_TYPES_H
#define COLOR_TYPES_H

typedef sensekit_streamconnection_t sensekit_colorstream_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bytesPerPixel;
} sensekit_colorframe_metadata_t;

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

typedef struct _sensekit_colorframe* sensekit_colorframe_t;

#endif // COLOR_TYPES_H
