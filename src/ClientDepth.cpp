
struct sensekit_depthframe_header_t {
    int frameIndex;
    int compressiontype;
};

struct _sensekit_depthframe {
    sensekit_depthframe_header_t header;
    short sampleValue;
};

typedef struct _sensekit_depthstream sensekit_depthstream_t;
typedef struct _sensekit_depthframe sensekit_depthframe_t;

SENSEKIT_API inline sensekit_status_t sensekit_depth_open(sensekit_streamset_t* sensor, /*out*/sensekit_depthstream_t** stream)
{
    sensekit_stream_t* sk_stream;
    sensekit_stream_open(sensor, &sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API inline sensekit_status_t sensekit_depth_close(/*inout*/sensekit_depthstream_t** stream)
{
    sensekit_stream_t* sk_stream = (sensekit_stream_t*)(*stream);

    sensekit_stream_close(&sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t** frame)
{
    sensekit_stream_t* sk_stream = (sensekit_stream_t*)(stream);
    sensekit_frame_t* sk_frame = NULL;

    sensekit_stream_frame_open(sk_stream, timeout, &sk_frame);
    
    sensekit_depthframe_header_t* header = (sensekit_depthframe_header_t*)(sk_frame->data);
    //interrogate header, optionally decompress, etc...

    if (header->compressed)
    {
        *frame = g_codec_decompress((sensekit_depthframe_compressed_t*)(sk_frame->data));
    }
    else
    {
        *frame = (sensekit_depthframe_t*)(sk_frame->data);
    }

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame)
{
    g_Context.close_depth_frame(frame);
    return SENSEKIT_STATUS_SUCCESS;
}
