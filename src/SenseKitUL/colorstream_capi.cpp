#include <sensekit_core.h>
#include "generic_stream_api.h"
#include <streams/color_types.h>
#include "SenseKitUL_internal.h"
#include <math.h>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_get(sensekit_reader_t* reader,
                                                     sensekit_colorstream_t** stream)
{
    return sensekit_generic_stream_get(reader,
                                       stream,
                                       SENSEKIT_STREAM_TYPE::SENSEKIT_STREAM_COLOR,
                                       DEFAULT_SUBTYPE);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_open(sensekit_colorstream_t* stream,
                                                            int timeoutMillis,
                                                            sensekit_colorframe_t** frame)
{
    return sensekit_generic_frame_open<sensekit_colorframe_wrapper_t>(stream, timeoutMillis, frame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_close(sensekit_colorframe_t** frame)
{
    return sensekit_generic_frame_close(frame);
}

SENSEKIT_END_DECLS
