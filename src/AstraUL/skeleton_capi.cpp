#include <Astra/astra_defines.h>
#include <Astra/astra_types.h>
#include <AstraUL/streams/skeleton_types.h>
#include "generic_stream_api.h"
#include <memory.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <string.h>


ASTRA_BEGIN_DECLS

ASTRA_API_EX astra_status_t astra_reader_get_skeletonstream(astra_reader_t reader,
                                                                     astra_skeletonstream_t* skeletonStream)

{
    return astra_reader_get_stream(reader,
                                      ASTRA_STREAM_SKELETON,
                                      DEFAULT_SUBTYPE,
                                      skeletonStream);
}

ASTRA_API_EX astra_status_t astra_frame_get_skeletonframe(astra_reader_frame_t readerFrame,
                                                                   astra_skeletonframe_t* skeletonFrame)
{
    return astra_generic_frame_get<astra_skeletonframe_wrapper_t>(readerFrame,
                                                                        ASTRA_STREAM_SKELETON,
                                                                        DEFAULT_SUBTYPE,
                                                                        skeletonFrame);
}

ASTRA_API_EX astra_status_t astra_skeletonframe_get_frameindex(astra_skeletonframe_t skeletonFrame,
                                                                        astra_frame_index_t* index)
{
    return astra_generic_frame_get_frameindex(skeletonFrame, index);
}

ASTRA_API_EX astra_status_t astra_skeletonframe_get_skeleton_count(astra_skeletonframe_t skeletonFrame,
                                                                            size_t* skeletonCount)
{
    *skeletonCount = skeletonFrame->skeletonCount;

    return ASTRA_STATUS_SUCCESS;
}

ASTRA_API_EX astra_status_t astra_skeletonframe_copy_skeletons(astra_skeletonframe_t skeletonFrame,
                                                                        astra_skeleton_t* skeletonsDestination)
{
    size_t size = skeletonFrame->skeletonCount * sizeof(astra_skeleton_t);

    memcpy(skeletonsDestination, skeletonFrame->skeletons, size);

    return ASTRA_STATUS_SUCCESS;
}

ASTRA_API_EX astra_status_t astra_skeletonframe_get_skeletons_ptr(astra_skeletonframe_t skeletonFrame,
                                                                           astra_skeleton_t** skeletons,
                                                                           size_t* skeletonCount)
{
    *skeletons = skeletonFrame->skeletons;
    astra_skeletonframe_get_skeleton_count(skeletonFrame, skeletonCount);

    return ASTRA_STATUS_SUCCESS;
}
ASTRA_END_DECLS
