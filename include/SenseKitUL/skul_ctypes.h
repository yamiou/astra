#ifndef SKUL_CTYPES_H
#define SKUL_CTYPES_H

#include <stdint.h>

typedef struct _sensekit_vector2i {
    int32_t x;
    int32_t y;
} sensekit_vector2i_t;

typedef struct _sensekit_vector2f {
    float x;
    float y;
} sensekit_vector2f_t;

typedef struct _sensekit_vector3f {
    float x;
    float y;
    float z;
} sensekit_vector3f_t;

typedef struct _sensekit_vector3i {
    int32_t x;
    int32_t y;
    int32_t z;
} sensekit_vector3i_t;

enum sensekit_core_stream_types {
    SENSEKIT_STREAM_DEPTH = 1,
    SENSEKIT_STREAM_COLOR = 2,
    SENSEKIT_STREAM_HAND = 3,
    SENSEKIT_STREAM_SKELETON = 4,
    SENSEKIT_STREAM_STYLIZED_DEPTH = 5,
    SENSEKIT_STREAM_DEBUG_HAND = 3001,
};

#endif /* SKUL_CTYPES_H */
