#ifndef STREAM_TYPES_H
#define STREAM_TYPES_H

#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/streams/image_types.h>

// https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
// http://stackoverflow.com/questions/3350852/how-to-correctly-fix-zero-sized-array-in-struct-union-warning-c4200-without

struct _sensekit_imageframe {
    sensekit_frame_t* frame;
    sensekit_image_metadata_t metadata;
    void* data;
};

struct _sensekit_handframe {
    sensekit_frame_t* frame;
    size_t handCount;
    sensekit_handpoint_t* handpoints;
};

#if defined(_MSC_VER)
#pragma warning( push )
#pragma warning( disable : 4200 )
#elsif defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc99-extensions"
#endif

typedef struct _sensekit_imageframe_wrapper {
    _sensekit_imageframe frame;
    char frame_data[];
} sensekit_imageframe_wrapper_t;

typedef struct _sensekit_handframe_wrapper {
    _sensekit_handframe frame;
    char frame_data[];
} sensekit_handframe_wrapper_t;

#if defined(_MSC_VER)
#pragma warning( pop )
#elsif defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

#endif /* STREAM_TYPES_H */
