#ifndef HANDS_CAPI_H
#define HANDS_CAPI_H

#include "hand_types.h"

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
                                                                    size_t* numHands);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_copy_hands(sensekit_handframe_t handFrame,
                                                                sensekit_handpoint_t* handPointsDestination);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_hands_ptr(sensekit_handframe_t handFrame,
                                                                   sensekit_handpoint_t** handpoints,
                                                                   size_t* numHands);

SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_debug_handstream(sensekit_reader_t reader,
                                                                       sensekit_debug_handstream_t* debugHandStream);

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_debug_handframe(sensekit_reader_frame_t readerFrame,
                                                                     sensekit_debug_handframe_t* debugHandFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_get_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t* viewType);

SENSEKIT_API_EX sensekit_status_t sensekit_debug_handstream_set_view_type(sensekit_debug_handstream_t debugHandStream,
                                                                          sensekit_debug_hand_view_type_t viewType);

SENSEKIT_END_DECLS

#endif // HANDS_CAPI_H
