#include <sensekit_core.h>
#include "generic_stream_api.h"
#include <streams/color_types.h>
#include "SenseKitUL_internal.h"
#include <math.h>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get(sensekit_reader_t* reader,
                                                           sensekit_colorstream_t** colorStream)

{
    return sensekit_generic_stream_get(reader,
                                       SENSEKIT_STREAM_COLOR,
                                       DEFAULT_SUBTYPE,
                                       colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_get(sensekit_reader_frame_t* readerFrame,
                                                           sensekit_colorframe_t** colorFrame)
{
    return sensekit_generic_frame_get<sensekit_colorframe_wrapper_t>(readerFrame,
                                                                     SENSEKIT_STREAM_COLOR,
                                                                     DEFAULT_SUBTYPE,
                                                                     colorFrame);
}
SENSEKIT_END_DECLS
