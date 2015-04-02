#ifndef COLOR_H
#define COLOR_H

#include <sensekit_core.h>
#include "color_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_get(sensekit_reader_t* reader,
                                                     sensekit_colorstream_t** stream);

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_open(sensekit_colorstream_t* stream,
                                                            int timeoutMillis,
                                                            sensekit_colorframe_t** frame);

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_close(sensekit_colorframe_t** frame);

SENSEKIT_END_DECLS

#endif // COLOR_H
