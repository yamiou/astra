#ifndef STREAMTYPES_H
#define STREAMTYPES_H

#include <sensekit_types.h>

enum SENSEKIT_STREAM_TYPE {
    SENSEKIT_STREAM_DEPTH = 1,
    SENSEKIT_STREAM_COLOR = 2
};

const sensekit_stream_subtype_t ANY_SUBTYPE = -1;
const sensekit_stream_subtype_t DEPTH_DEFAULT_SUBTYPE = 0;
const sensekit_stream_subtype_t COLOR_DEFAULT_SUBTYPE = 0;

#endif /* STREAMTYPES_H */
