#ifndef STREAM_TYPES_H
#define STREAM_TYPES_H

#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/streams/image_types.h>

// https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
// http://stackoverflow.com/questions/3350852/how-to-correctly-fix-zero-sized-array-in-struct-union-warning-c4200-without

struct _sensekit_imageframe {
    sensekit_frame_ref_t* frameRef;
    sensekit_image_metadata_t metadata;
    void* data;
};

struct _sensekit_handframe {
    sensekit_frame_ref_t* frameRef;
    size_t handCount;
    sensekit_handpoint_t* handpoints;
};

#pragma warning( push )
#pragma warning( disable : 4200 )

typedef struct _sensekit_imageframe_wrapper {
    _sensekit_imageframe frame;
    char frame_data[];
} sensekit_imageframe_wrapper_t;

typedef struct _sensekit_handframe_wrapper {
    _sensekit_handframe frame;
    char frame_data[];
} sensekit_handframe_wrapper_t;

#pragma warning( pop )

#endif /* STREAM_TYPES_H */
