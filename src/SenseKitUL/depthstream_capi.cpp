#include <sensekit_core.h>
#include "generic_stream_api.h"
#include <streams/depth_types.h>
#include <math.h>
#include <StreamTypes.h>
#include "SenseKitUL_internal.h"

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

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                         float* pWorldX, float* pWorldY, float* pWorldZ)
{
    float normalizedX = depthX / g_conversionCache.resolutionX - .5f;
    float normalizedY = .5f - depthY / g_conversionCache.resolutionY;

    *pWorldX = normalizedX * depthZ * g_conversionCache.xzFactor;
    *pWorldY = normalizedY * depthZ * g_conversionCache.yzFactor;
    *pWorldZ = depthZ;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t convert_world_to_depth(float worldX, float worldY, float worldZ,
                                                         float* pDepthX, float* pDepthY, float* pDepthZ)
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

SENSEKIT_API_EX sensekit_status_t sensekit_depth_get(sensekit_reader_t* reader,
                                                     sensekit_depthstream_t** stream)
{
    refresh_conversion_cache();
    return sensekit_generic_stream_get(reader,
                                       stream,
                                       SENSEKIT_STREAM_TYPE::SENSEKIT_STREAM_DEPTH,
                                       DEFAULT_SUBTYPE);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream,
                                                            int timeoutMillis,
                                                            sensekit_depthframe_t** frame)
{
    return sensekit_generic_frame_open<sensekit_depthframe_wrapper_t>(stream, timeoutMillis, frame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t** frame)
{
    return sensekit_generic_frame_close(frame);
}

SENSEKIT_END_DECLS
