#include <sensekit_types.h>
#include "generic_stream_api.h"
#include <streams/depth_types.h>
#include <math.h>
#include <memory.h>
#include <StreamTypes.h>
#include "SenseKitUL_internal.h"
#include <streams/depth_capi.h>

static conversion_cache_t g_conversionCache;

static void refresh_conversion_cache()
{
    // the hardest of codings PART DEUX

    static bool is_cached = false;

    if (is_cached)
        return;

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

    is_cached = true;
}

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t convert_depth_to_world(float depthX, float depthY, float depthZ,
                                                         float* pWorldX, float* pWorldY, float* pWorldZ)
{
    refresh_conversion_cache();

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
    refresh_conversion_cache();

    *pDepthX = g_conversionCache.coeffX * worldX / worldZ + g_conversionCache.halfResX;
    *pDepthY = g_conversionCache.halfResY - g_conversionCache.coeffY * worldY / worldZ;
    *pDepthZ = worldZ;

    return SENSEKIT_STATUS_SUCCESS;
}


SENSEKIT_API_EX sensekit_status_t sensekit_depth_stream_get(sensekit_reader_t reader,
                                                            sensekit_depthstream_t* depthStream)

{
    return sensekit_generic_stream_get(reader,
                                       SENSEKIT_STREAM_DEPTH,
                                       DEFAULT_SUBTYPE,
                                       depthStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_depthframe_t* depthFrame)
{
    return sensekit_generic_frame_get<sensekit_depthframe_wrapper_t>(readerFrame,
                                                                     SENSEKIT_STREAM_DEPTH,
                                                                     DEFAULT_SUBTYPE,
                                                                     depthFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_frameindex(sensekit_depthframe_t depthFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(depthFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_length(sensekit_depthframe_t depthFrame,
                                                                      size_t* length)
{
    sensekit_depthframe_metadata_t metadata = depthFrame->metadata;

    size_t size = metadata.width * metadata.height * metadata.bytesPerPixel;
    *length = size;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_ptr(sensekit_depthframe_t depthFrame,
                                                                   int16_t** data,
                                                                   size_t* length)
{
    *data = depthFrame->data;
    sensekit_depthframe_get_data_length(depthFrame, length);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_copy_data(sensekit_depthframe_t depthFrame,
                                                                int16_t* data)
{
    sensekit_depthframe_metadata_t metadata = depthFrame->metadata;
    size_t size = metadata.width * metadata.height * metadata.bytesPerPixel;

    memcpy(data, depthFrame->data, size);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_metadata(sensekit_depthframe_t depthFrame,
                                                                   sensekit_depthframe_metadata_t* metadata )
{
    *metadata = depthFrame->metadata;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
