#ifndef HAND_TYPES_H
#define HAND_TYPES_H

#define SENSEKIT_HANDS_MAX_HANDPOINTS 20

typedef enum _sensekit_handstatus
{
    HAND_STATUS_NOTTRACKING,
    HAND_STATUS_TRACKING,
    HAND_STATUS_LOST
} sensekit_handstatus_t;

typedef struct _sensekit_vector2i {
    int32_t x;
    int32_t y;
} sensekit_vector2i_t;

typedef struct _sensekit_vector3f {
    float x;
    float y;
    float z;
} sensekit_vector3f_t;

typedef struct _sensekit_handpoint {
    int32_t trackingId;
    sensekit_handstatus_t status;
    sensekit_vector2i_t depthPosition;
    sensekit_vector3f_t worldPosition;
    sensekit_vector3f_t worldDeltaPosition;
} sensekit_handpoint_t;

typedef struct _sensekit_handframe* sensekit_handframe_t;

typedef struct _sensekit_handstream* sensekit_handstream_t;

#endif // HAND_TYPES_H
