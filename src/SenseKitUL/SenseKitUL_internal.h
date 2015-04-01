#ifndef SENSEKITUL_INTERNAL_H
#define SENSEKITUL_INTERNAL_H

#include <streams/depth_types.h>
#include <streams/color_types.h>
#include <streams/hand_types.h>

// https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html

// http://stackoverflow.com/questions/3350852/how-to-correctly-fix-zero-sized-array-in-struct-union-warning-c4200-without
#pragma warning( push )
#pragma warning( disable : 4200 )
typedef struct _sensekit_depthframe_wrapper {
    sensekit_depthframe_t frame;
    char frame_data[];
} sensekit_depthframe_wrapper_t;

typedef struct _sensekit_colorframe_wrapper {
    sensekit_colorframe_t frame;
    char frame_data[];
} sensekit_colorframe_wrapper_t;
#pragma warning( pop )

typedef struct _sensekit_handframe_wrapper {
    sensekit_handframe_t frame;
    char frame_data[];
} sensekit_handframe_wrapper_t;

#endif /* SENSEKITUL_INTERNAL_H */
