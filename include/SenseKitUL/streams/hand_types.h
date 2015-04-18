#ifndef HAND_TYPES_H
#define HAND_TYPES_H

#include <SenseKit/sensekit_types.h>
#include <SenseKitUL/cvector.h>

#define SENSEKIT_HANDS_MAX_HANDPOINTS 20

typedef enum _sensekit_handstatus
{
    HAND_STATUS_NOTTRACKING,
    HAND_STATUS_TRACKING,
    HAND_STATUS_LOST
} sensekit_handstatus_t;

typedef struct _sensekit_handpoint {
    int32_t trackingId;
    sensekit_handstatus_t status;
    sensekit_vector2i_t depthPosition;
    sensekit_vector3f_t worldPosition;
    sensekit_vector3f_t worldDeltaPosition;
} sensekit_handpoint_t;

typedef struct _sensekit_handframe* sensekit_handframe_t;
typedef sensekit_streamconnection_t sensekit_handstream_t;

typedef struct _sensekit_imageframe* sensekit_debug_handframe_t;
typedef sensekit_streamconnection_t sensekit_debug_handstream_t;

#endif // HAND_TYPES_H
