#ifndef COLOR_H
#define COLOR_H

#include <SenseKit.h>
#include "color_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_open(sensekit_streamset_t* sensor,
                                                   sensekit_colorstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_color_close(sensekit_colorstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_open(sensekit_colorstream_t* stream,
                                                         int timeout, sensekit_colorframe_t** frame);

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_close(sensekit_colorframe_t** frame);

SENSEKIT_END_DECLS

#endif // COLOR_H
