#include <SenseKit/sensekit_defines.h>
#include <SenseKit/sensekit_types.h>
#include <SenseKitUL/streams/skeleton_types.h>
#include "generic_stream_api.h"
#include <memory.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <string.h>


SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_skeletonstream(sensekit_reader_t reader,
                                                                     sensekit_skeletonstream_t* skeletonStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_SKELETON,
                                      DEFAULT_SUBTYPE,
                                      skeletonStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_skeletonframe(sensekit_reader_frame_t readerFrame,
                                                                   sensekit_skeletonframe_t* skeletonFrame)
{
    return sensekit_generic_frame_get<sensekit_skeletonframe_wrapper_t>(readerFrame,
                                                                        SENSEKIT_STREAM_SKELETON,
                                                                        DEFAULT_SUBTYPE,
                                                                        skeletonFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_frameindex(sensekit_skeletonframe_t skeletonFrame,
                                                                        sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(skeletonFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_skeleton_count(sensekit_skeletonframe_t skeletonFrame,
                                                                            size_t* skeletonCount)
{
    *skeletonCount = skeletonFrame->skeletonCount;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_copy_skeletons(sensekit_skeletonframe_t skeletonFrame,
                                                                        sensekit_skeleton_t* skeletonsDestination)
{
    size_t size = skeletonFrame->skeletonCount * sizeof(sensekit_skeleton_t);

    memcpy(skeletonsDestination, skeletonFrame->skeletons, size);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_skeletonframe_get_skeletons_ptr(sensekit_skeletonframe_t skeletonFrame,
                                                                           sensekit_skeleton_t** skeletons,
                                                                           size_t* skeletonCount)
{
    *skeletons = skeletonFrame->skeletons;
    sensekit_skeletonframe_get_skeleton_count(skeletonFrame, skeletonCount);

    return SENSEKIT_STATUS_SUCCESS;
}
SENSEKIT_END_DECLS
