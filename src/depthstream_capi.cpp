#include <SenseKit.h>
#include <streams/depth_types.h>
#include <math.h>

static WorldConversionCache g_convertCache;

SENSEKIT_BEGIN_DECLS

sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ, float* pWorldX, float* pWorldY, float* pWorldZ)
{
    float normalizedX = depthX / g_convertCache.resolutionX - .5f;
    float normalizedY = .5f - depthY / g_convertCache.resolutionY;

    *pWorldX = normalizedX * depthZ * g_convertCache.xzFactor;
    *pWorldY = normalizedY * depthZ * g_convertCache.yzFactor;
    *pWorldZ = depthZ;

    return SENSEKIT_STATUS_SUCCESS;
}

sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ, float* pDepthX, float* pDepthY, float* pDepthZ)
{
    *pDepthX = g_convertCache.coeffX * worldX / worldZ + g_convertCache.halfResX;
    *pDepthY = g_convertCache.halfResY - g_convertCache.coeffY * worldY / worldZ;
    *pDepthZ = worldZ;

    return SENSEKIT_STATUS_SUCCESS;
}

static void refresh_conversion_cache()
{
    // the hardest of codings
    float horizontalFov = 58.0f;
    float verticalFov = 45.0f;
    int resolutionX = 320;
    int resolutionY = 240;

    g_convertCache.xzFactor = tan(horizontalFov / 2) * 2;
    g_convertCache.yzFactor = tan(verticalFov / 2) * 2;
    g_convertCache.resolutionX = resolutionX;
    g_convertCache.resolutionY = resolutionY;
    g_convertCache.halfResX = g_convertCache.resolutionX / 2;
    g_convertCache.halfResY = g_convertCache.resolutionY / 2;
    g_convertCache.coeffX = g_convertCache.resolutionX / g_convertCache.xzFactor;
    g_convertCache.coeffY = g_convertCache.resolutionY / g_convertCache.yzFactor;
}

sensekit_status_t sensekit_depth_open(sensekit_streamset_t* sensor, /*out*/sensekit_depthstream_t** stream)
{
    refresh_conversion_cache();

    sensekit_stream_t* sk_stream;
    sensekit_stream_open(sensor, &sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

sensekit_status_t sensekit_depth_close(/*inout*/sensekit_depthstream_t** stream)
{
    sensekit_stream_t* sk_stream = (sensekit_stream_t*)(*stream);

    sensekit_stream_close(&sk_stream);

    *stream = (sensekit_depthstream_t*)sk_stream;
    return SENSEKIT_STATUS_SUCCESS;
}

sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t** frame)
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

sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame)
{
    sensekit_frame_ref_t* sk_frameRef = (*frame)->frameRef;

    sensekit_stream_frame_close(&sk_frameRef);

    *frame = nullptr;

    return SENSEKIT_STATUS_SUCCESS;
}
SENSEKIT_END_DECLS
