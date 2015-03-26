#include <SenseKit.h>
#include <streams/depth_types.h>
#include <math.h>
#include <sensekit_known_streams.h>

struct coonversion_cache_t
{
    float xzFactor;
    float yzFactor;
    float coeffX;
    float coeffY;
    int resolutionX;
    int resolutionY;
    int halfResX;
    int halfResY;
};

static coonversion_cache_t g_conversionCache;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ, float* pWorldX, float* pWorldY, float* pWorldZ)
{
    float normalizedX = depthX / g_conversionCache.resolutionX - .5f;
    float normalizedY = .5f - depthY / g_conversionCache.resolutionY;

    *pWorldX = normalizedX * depthZ * g_conversionCache.xzFactor;
    *pWorldY = normalizedY * depthZ * g_conversionCache.yzFactor;
    *pWorldZ = depthZ;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ, float* pDepthX, float* pDepthY, float* pDepthZ)
{
    *pDepthX = g_conversionCache.coeffX * worldX / worldZ + g_conversionCache.halfResX;
    *pDepthY = g_conversionCache.halfResY - g_conversionCache.coeffY * worldY / worldZ;
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

    g_conversionCache.xzFactor = tan(horizontalFov / 2) * 2;
    g_conversionCache.yzFactor = tan(verticalFov / 2) * 2;
    g_conversionCache.resolutionX = resolutionX;
    g_conversionCache.resolutionY = resolutionY;
    g_conversionCache.halfResX = g_conversionCache.resolutionX / 2;
    g_conversionCache.halfResY = g_conversionCache.resolutionY / 2;
    g_conversionCache.coeffX = g_conversionCache.resolutionX / g_conversionCache.xzFactor;
    g_conversionCache.coeffY = g_conversionCache.resolutionY / g_conversionCache.yzFactor;
}

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_streamset_t* streamset, /*out*/sensekit_depthstream_t** stream)
{
    refresh_conversion_cache();

    sensekit_streamconnection_t* stream_connection;
    sensekit_stream_open(streamset, DEPTH_TYPE, ANY_SUBTYPE, &stream_connection);

    sensekit_stream_set_parameter(stream_connection, 2, 4, nullptr);
    size_t len;
    sensekit_stream_get_parameter_size(stream_connection, 3, &len);

    *stream = (sensekit_depthstream_t*)stream_connection;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_close(/*inout*/sensekit_depthstream_t** stream)
{
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)(*stream);

    sensekit_stream_close(&sk_stream_connection);

    *stream = (sensekit_depthstream_t*)sk_stream_connection;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)(stream);

    sensekit_stream_frame_open(sk_stream_connection, timeout, &frameRef);

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

SENSEKIT_API sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame)
{
    sensekit_frame_ref_t* sk_frameRef = (*frame)->frameRef;

    sensekit_stream_frame_close(&sk_frameRef);

    *frame = nullptr;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
