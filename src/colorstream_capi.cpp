#include <SenseKit.h>
#include <streams/color_types.h>
#include <math.h>
#include <sensekit_known_streams.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_color_open(sensekit_streamset_t* streamset, /*out*/sensekit_colorstream_t** stream)
{
    sensekit_streamconnection_t* stream_connection;
    sensekit_stream_open(streamset, COLOR_TYPE, ANY_SUBTYPE, &stream_connection);

    *stream = (sensekit_colorstream_t*)stream_connection;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_color_close(/*inout*/sensekit_colorstream_t** stream)
{
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)(*stream);

    sensekit_stream_close(&sk_stream_connection);

    *stream = (sensekit_colorstream_t*)sk_stream_connection;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_color_frame_open(sensekit_colorstream_t* stream, int timeout, sensekit_colorframe_t** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)stream;

    sensekit_stream_frame_open(sk_stream_connection, timeout, &frameRef);

    sensekit_colorframe_wrapper_t* wrapper = (sensekit_colorframe_wrapper_t*)(frameRef->frame->data);
    *frame = (sensekit_colorframe_t*)(&(wrapper->frame));
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_color_frame_close(sensekit_colorframe_t** frame)
{
    sensekit_frame_ref_t* sk_frameRef = (*frame)->frameRef;

    sensekit_stream_frame_close(&sk_frameRef);

    *frame = nullptr;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
