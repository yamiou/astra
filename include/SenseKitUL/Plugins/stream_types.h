#ifndef SENSEKITUL_INTERNAL_H
#define SENSEKITUL_INTERNAL_H

#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/streams/hand_types.h>

// https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
// http://stackoverflow.com/questions/3350852/how-to-correctly-fix-zero-sized-array-in-struct-union-warning-c4200-without

struct _sensekit_depthframe {
    sensekit_frame_ref_t* frameRef;
    sensekit_depthframe_metadata_t metadata;
    int16_t* data;
};

struct _sensekit_colorframe {
    sensekit_frame_ref_t* frameRef;
    sensekit_colorframe_metadata_t metadata;
    uint8_t* data;
};

struct _sensekit_handframe {
    sensekit_frame_ref_t* frameRef;
    size_t numHands;
    sensekit_handpoint_t* handpoints;
};

#pragma warning( push )
#pragma warning( disable : 4200 )

typedef struct _sensekit_depthframe_wrapper {
    _sensekit_depthframe frame;
    char frame_data[];
} sensekit_depthframe_wrapper_t;

typedef struct _sensekit_colorframe_wrapper {
    _sensekit_colorframe frame;
    char frame_data[];
} sensekit_colorframe_wrapper_t;

typedef struct _sensekit_handframe_wrapper {
    _sensekit_handframe frame;
    char frame_data[];
} sensekit_handframe_wrapper_t;

#pragma warning( pop )

#endif /* SENSEKITUL_INTERNAL_H */
