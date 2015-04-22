#ifndef SKELETON_TYPES_H
#define SKELETON_TYPES_H

#include <SenseKit/sensekit_types.h>
#include <SenseKitUL/skul_ctypes.h>

enum sensekit_joint_type {
    SENSEKIT_JOINT_TYPE_LEFT_SHOULDER = 1,
    SENSEKIT_JOINT_TYPE_LEFT_ELBOW = 2,
    SENSEKIT_JOINT_TYPE_LEFT_HAND = 3,
    SENSEKIT_JOINT_TYPE_RIGHT_SHOULDER = 4,
    SENSEKIT_JOINT_TYPE_RIGHT_ELBOW = 5,
    SENSEKIT_JOINT_TYPE_RIGHT_HAND = 6,
    SENSEKIT_JOINT_TYPE_HEAD = 7,
    SENSEKIT_JOINT_TYPE_HIP = 8
};

enum sensekit_skeleton_status {
    SENSEKIT_SKELETON_STATUS_NOT_TRACKED = 0,
    SENSEKIT_SKELETON_STATUS_TRACKED = 1
};

enum sensekit_joint_status {
    SENSEKIT_JOINT_STATUS_NOT_TRACKED = 0,
    SENSEKIT_JOINT_STATUS_TRACKED = 1
};

const size_t SENSEKIT_MAX_JOINTS = 20;

typedef struct _sensekit_skeleton_joint {
    int32_t trackingId;
    sensekit_joint_type status;
    sensekit_joint_type jointType;
    sensekit_vector3f_t position;
} sensekit_skeleton_joint_t;

typedef struct _sensekit_skeleton {
    int32_t trackingId;
    size_t jointCount;
    sensekit_skeleton_status status;
    sensekit_skeleton_joint_t joints[SENSEKIT_MAX_JOINTS];
} sensekit_skeleton_t;

struct _sensekit_skeletonframe {
    sensekit_frame_ref_t* frameRef;
    size_t skeletonCount;
    sensekit_skeleton_t* skeletons;
};

#pragma warning( push )
#pragma warning( disable : 4200 )

typedef struct _sensekit_skeletonframe_wrapper {
    _sensekit_skeletonframe frame;
    char frame_data[];
} sensekit_skeletonframe_wrapper_t;

#pragma  warning( pop )

typedef struct _sensekit_skeletonframe* sensekit_skeletonframe_t;
typedef sensekit_streamconnection_t sensekit_skeletonstream_t;

typedef struct _sensekit_imageframe* sensekit_debug_skeletonframe_t;
typedef sensekit_streamconnection_t sensekit_debug_skeletonstream_t;

#endif /* SKELETON_TYPES_H */
