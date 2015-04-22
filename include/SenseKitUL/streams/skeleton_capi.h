#ifndef SKELETON_CAPI_H
#define SKELETON_CAPI_H

#include <SenseKit/sensekit_defines.h>
#include <SenseKit/sensekit_types.h>
#include "skeleton_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_skeletonstream(sensekit_reader_t reader,
                                                                     sensekit_skeletonstream_t* skeletonStream);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_skeletonframe(sensekit_reader_frame_t readerFrame,
                                                                   sensekit_skeletonframe_t* skeletonFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_frameindex(sensekit_skeletonframe_t skeletonFrame,
                                                                        sensekit_frame_index_t* index);

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_skeleton_count(sensekit_skeletonframe_t skeletonFrame,
                                                                            size_t* skeletonCount);

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_copy_skeletons(sensekit_skeletonframe_t skeletonFrame,
                                                                        sensekit_skeleton_t* skeletonsDestination);

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_skeletons_ptr(sensekit_skeletonframe_t skeletonFrame,
                                                                           sensekit_skeleton_t** skeletons,
                                                                           size_t* skeletonCount);

SENSEKIT_END_DECLS

#endif // SKELETON_CAPI_H
