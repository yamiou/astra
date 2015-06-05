#ifndef HAND_CAPI_H
#define HAND_CAPI_H

#include <SenseKit/sensekit_defines.h>
#include <SenseKit/sensekit_types.h>
#include "hand_types.h"
#include <stdbool.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_handstream(sensekit_reader_t reader,
                                                                 sensekit_handstream_t* handStream);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_handframe(sensekit_reader_frame_t readerFrame,
                                                               sensekit_handframe_t* handFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_handframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                       sensekit_stream_subtype_t subtype,
                                                                       sensekit_handframe_t* handFrame);
SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_frameindex(sensekit_handframe_t handFrame,
                                                                    sensekit_frame_index_t* index);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_hand_count(sensekit_handframe_t handFrame,
                                                                    size_t* handCount);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_copy_hands(sensekit_handframe_t handFrame,
                                                                sensekit_handpoint_t* handPointsDestination);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_shared_hand_array(sensekit_handframe_t handFrame,
                                                                           sensekit_handpoint_t** handpoints,
                                                                           size_t* handCount);

SENSEKIT_API_EX sensekit_status_t sensekit_handstream_get_include_candidate_points(sensekit_handstream_t handStream,
                                                                                   bool* includeCandidatePoints);

SENSEKIT_API_EX sensekit_status_t sensekit_handstream_set_include_candidate_points(sensekit_handstream_t handStream,
                                                                                   bool includeCandidatePoints);

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_debug_handstream(sensekit_reader_t reader,
                                                                       sensekit_debug_handstream_t* debugHandStream);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_debug_handframe(sensekit_reader_frame_t readerFrame,
                                                                     sensekit_debug_handframe_t* debugHandFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_get_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t* viewType);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t viewType);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_mouse_position(sensekit_debug_handstream_t debugHandStream,
                                                                               sensekit_vector2f_t normPosition);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_use_mouse_probe(sensekit_debug_handstream_t debugHandStream,
                                                                                bool useMouseProbe);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_pause_input(sensekit_debug_handstream_t debugHandStream,
                                                                            bool pauseInput);

SENSEKIT_END_DECLS

#endif // HAND_CAPI_H
