#ifndef DEPTH_H
#define DEPTH_H

#include <SenseKit.h>
#include "depth_types.h"

SENSEKIT_BEGIN_DECLS

static inline sensekit_status_t sensekit_depth_open(sensekit_streamset_t* sensor, /*out*/sensekit_depthstream_t** stream)
{
    sensekit_stream_t* sk_stream;
    sensekit_stream_open(sensor, &sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

static inline sensekit_status_t sensekit_depth_close(/*inout*/sensekit_depthstream_t** stream)
{
    sensekit_stream_t* sk_stream = (sensekit_stream_t*)(*stream);

    sensekit_stream_close(&sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

static inline sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_stream_t* skStream = (sensekit_stream_t*)(stream);

    sensekit_stream_frame_open(skStream, timeout, &frameRef);

    //SOOON...
    //sensekit_depthframe_header_t* header = (sensekit_depthframe_header_t*)(sk_frame->data);
    //interrogate header, optionally decompress, etc...

    //if (header->compressed)
    //{
    //*frame = codec_decompress((sensekit_depthframe_compressed_t*)(sk_frame->data));
    //}
    //else
    //{
    sensekit_depthframe_wrapper_t* wrapper = (sensekit_depthframe_wrapper_t*)(frameRef->frame->data);
    *frame = (sensekit_depthframe_t*)&(wrapper->frame);
    (*frame)->frameRef = frameRef;
    //}

    return SENSEKIT_STATUS_SUCCESS;
}

static inline sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame)
{
    sensekit_frame_ref_t* sk_frameRef = (*frame)->frameRef;

    sensekit_stream_frame_close(&sk_frameRef);

    *frame = nullptr;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS

#endif // DEPTH_H
