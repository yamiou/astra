#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <memory.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/streams/hand_capi.h>
#include <string.h>
#include <SenseKitUL/streams/image_capi.h>
#include <SenseKitUL/streams/hand_parameters.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_handstream(sensekit_reader_t reader,
                                                           sensekit_handstream_t* handStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_HAND,
                                      DEFAULT_SUBTYPE,
                                      handStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_handframe(sensekit_reader_frame_t readerFrame,
                                                          sensekit_handframe_t* handFrame)
{
    return sensekit_generic_frame_get<sensekit_handframe_wrapper_t>(readerFrame,
                                                                    SENSEKIT_STREAM_HAND,
                                                                    DEFAULT_SUBTYPE,
                                                                    handFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_handframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                            sensekit_stream_subtype_t subtype,
                                                                            sensekit_handframe_t* handFrame)
{
    return sensekit_generic_frame_get<sensekit_handframe_wrapper_t>(readerFrame,
                                                                    SENSEKIT_STREAM_HAND,
                                                                    subtype,
                                                                    handFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_frameindex(sensekit_handframe_t handFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(handFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_hand_count(sensekit_handframe_t handFrame,
                                                                   size_t* handCount)
{
    *handCount = handFrame->handCount;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_copy_hands(sensekit_handframe_t handFrame,
                                                                sensekit_handpoint_t* handPointsDestination)
{
    size_t size = handFrame->handCount * sizeof(sensekit_handpoint_t);

    memcpy(handPointsDestination, handFrame->handpoints, size);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_shared_hand_array(sensekit_handframe_t handFrame,
                                                                           sensekit_handpoint_t** handpoints,
                                                                           size_t* handCount)
{
    *handpoints = handFrame->handpoints;
    sensekit_handframe_get_hand_count(handFrame, handCount);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_debug_handstream(sensekit_reader_t reader,
                                                                       sensekit_debug_handstream_t* debugHandStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_DEBUG_HAND,
                                      DEFAULT_SUBTYPE,
                                      debugHandStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_debug_handframe(sensekit_reader_frame_t readerFrame,
                                                                     sensekit_debug_handframe_t* debugHandFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_DEBUG_HAND,
                                          DEFAULT_SUBTYPE,
                                          debugHandFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_get_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t* viewType)
{
    return sensekit_stream_get_parameter_fixed(debugHandStream,
                                               SENSEKIT_PARAMETER_DEBUG_HAND_VIEW_TYPE,
                                               sizeof(sensekit_debug_hand_view_type_t),
                                               reinterpret_cast<sensekit_parameter_data_t*>(viewType));
}

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t viewType)
{
    return sensekit_stream_set_parameter(debugHandStream,
                                         SENSEKIT_PARAMETER_DEBUG_HAND_VIEW_TYPE,
                                         sizeof(sensekit_debug_hand_view_type_t),
                                         reinterpret_cast<sensekit_parameter_data_t>(&viewType));
}

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_mouse_position(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_vector2f_t normPosition)
{
    return sensekit_stream_set_parameter(debugHandStream,
                                         SENSEKIT_PARAMETER_DEBUG_HAND_MOUSE_NORM_POSITION,
                                         sizeof(sensekit_vector2f_t),
                                         reinterpret_cast<sensekit_parameter_data_t>(&normPosition));
}

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_use_mouse_probe(sensekit_debug_handstream_t debugHandStream,
                                                                          bool useMouseProbe)
{
    return sensekit_stream_set_parameter(debugHandStream,
                                         SENSEKIT_PARAMETER_DEBUG_HAND_USE_MOUSE_PROBE,
                                         sizeof(bool),
                                         reinterpret_cast<sensekit_parameter_data_t>(&useMouseProbe));
}

SENSEKIT_END_DECLS
