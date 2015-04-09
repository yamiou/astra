#ifndef STREAMTYPES_H
#define STREAMTYPES_H

#include <sensekit_types.h>

enum sensekit_core_stream_types {
    SENSEKIT_STREAM_DEPTH = 1,
    SENSEKIT_STREAM_COLOR = 2,
    SENSEKIT_STREAM_HAND = 3
};

const sensekit_stream_subtype_t DEPTH_DEFAULT_SUBTYPE = 0;
const sensekit_stream_subtype_t COLOR_DEFAULT_SUBTYPE = 0;
const sensekit_stream_subtype_t HAND_DEFAULT_SUBTYPE = 0;

#endif /* STREAMTYPES_H */
