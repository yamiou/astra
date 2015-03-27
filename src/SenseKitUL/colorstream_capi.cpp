#include <SenseKit.h>
#include "generic_stream_api.h"
#include <streams/color_types.h>
#include "SenseKitUL_internal.h"
#include <math.h>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_color_open(sensekit_streamset_t* streamset,
                                                   sensekit_colorstream_t** stream)
{
    return sensekit_generic_stream_open(streamset, stream,
                                        SENSEKIT_STREAM_TYPE::SENSEKIT_STREAM_COLOR,
                                        ANY_SUBTYPE);
}

SENSEKIT_API sensekit_status_t sensekit_color_close(sensekit_colorstream_t** stream)
{
    return sensekit_generic_stream_close(stream);
}

SENSEKIT_API sensekit_status_t sensekit_color_frame_open(sensekit_colorstream_t* stream,
                                                         int timeout,
                                                         sensekit_colorframe_t** frame)
{
    return sensekit_generic_frame_open<sensekit_colorframe_wrapper_t>(stream, timeout, frame);
}

SENSEKIT_API sensekit_status_t sensekit_color_frame_close(sensekit_colorframe_t** frame)
{
    return sensekit_generic_frame_close(frame);
}

SENSEKIT_END_DECLS
