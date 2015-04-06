#ifndef COLOR_H
#define COLOR_H

#include <sensekit_core.h>
#include "color_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get(sensekit_reader_t reader,
                                                           sensekit_colorstream_t* colorStream);

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_colorframe_t** colorFrame);

SENSEKIT_END_DECLS

#endif // COLOR_H
