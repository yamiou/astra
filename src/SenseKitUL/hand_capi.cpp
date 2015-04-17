#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <math.h>
#include <memory.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/streams/hand_capi.h>
#include <string.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_hand_get_stream(sensekit_reader_t reader,
                                                           sensekit_handstream_t* handStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_HAND,
                                      DEFAULT_SUBTYPE,
                                      handStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_hand_debug_image_get_stream(sensekit_reader_t reader,
                                                                       sensekit_colorstream_t* handDebugImageStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_HAND_DEBUG_IMAGE,
                                      DEFAULT_SUBTYPE,
                                      handDebugImageStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_hand_get_frame(sensekit_reader_frame_t readerFrame,
                                                          sensekit_handframe_t* handFrame)
{
    return sensekit_generic_frame_get<sensekit_handframe_wrapper_t>(readerFrame,
                                                                    SENSEKIT_STREAM_HAND,
                                                                    DEFAULT_SUBTYPE,
                                                                    handFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_hand_debug_image_get_frame(sensekit_reader_frame_t readerFrame,
                                                                      sensekit_colorframe_t* handDebugImageFrame)
{
    return sensekit_generic_frame_get<sensekit_handframe_wrapper_t>(readerFrame,
                                                                    SENSEKIT_STREAM_HAND_DEBUG_IMAGE,
                                                                    DEFAULT_SUBTYPE,
                                                                    handDebugImageFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_frameindex(sensekit_handframe_t handFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(handFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_num_hands(sensekit_handframe_t handFrame,
                                                                   size_t* numHands)
{
    *numHands = handFrame->numHands;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_copy_hands(sensekit_handframe_t handFrame,
                                                                sensekit_handpoint_t* handPointsDestination)
{
    size_t size = handFrame->numHands * sizeof(sensekit_handpoint_t);

    memcpy(handPointsDestination, handFrame->handpoints, size);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_hands_ptr(sensekit_handframe_t handFrame,
    sensekit_handpoint_t** handpoints,
    size_t* numHands)
{
    *handpoints = handFrame->handpoints;
    sensekit_handframe_get_num_hands(handFrame, numHands);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
