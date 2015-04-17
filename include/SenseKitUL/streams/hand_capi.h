#ifndef HANDS_CAPI_H
#define HANDS_CAPI_H

#include "hand_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_hand_get_stream(sensekit_reader_t reader,
                                                           sensekit_handstream_t* handStream);

SENSEKIT_API_EX sensekit_status_t sensekit_hand_debug_image_get_stream(sensekit_reader_t reader,
                                                                       sensekit_colorstream_t* handDebugImageStream);

SENSEKIT_API_EX sensekit_status_t sensekit_hand_get_frame(sensekit_reader_frame_t readerFrame,
                                                          sensekit_handframe_t* handFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_hand_get_frame_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                       sensekit_stream_subtype_t subtype,
                                                                       sensekit_handframe_t* handFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_hand_debug_image_get_frame(sensekit_reader_frame_t readerFrame,
                                                                      sensekit_colorframe_t* handDebugImageFrame);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_frameindex(sensekit_handframe_t handFrame,
                                                                     sensekit_frame_index_t* index);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_num_hands(sensekit_handframe_t handFrame,
                                                                   size_t* numHands);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_copy_hands(sensekit_handframe_t handFrame,
                                                                sensekit_handpoint_t* handPointsDestination);

SENSEKIT_API_EX sensekit_status_t sensekit_handframe_get_hands_ptr(sensekit_handframe_t handFrame,
                                                                   sensekit_handpoint_t** handpoints,
                                                                   size_t* numHands);

SENSEKIT_END_DECLS

#endif // HANDS_CAPI_H
