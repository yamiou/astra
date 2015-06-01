#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/depth_types.h>
#include <memory.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/streams/depth_parameters.h>
#include <string.h>
#include <SenseKitUL/streams/image_capi.h>
#include <SenseKitUL/streams/image_parameters.h>
#include <unordered_map>
#include <Shiny.h>

using ConversionMap = std::unordered_map <sensekit_depthstream_t, conversion_cache_t>;

ConversionMap g_sensekit_conversion_map;

conversion_cache_t sensekit_depth_fetch_conversion_cache(sensekit_depthstream_t depthStream)
{
    PROFILE_FUNC();
    auto it = g_sensekit_conversion_map.find(depthStream);

    if (it != g_sensekit_conversion_map.end())
    {
        return it->second;
    }
    else
    {
        PROFILE_BEGIN(depth_cache_get);
        conversion_cache_t conversionCache;
        sensekit_stream_get_parameter_fixed(depthStream,
                                            SENSEKIT_PARAMETER_DEPTH_CONVERSION_CACHE,
                                            sizeof(conversion_cache_t),
                                            reinterpret_cast<sensekit_parameter_data_t*>(&conversionCache));
        g_sensekit_conversion_map.insert(std::make_pair(depthStream, conversionCache));
        PROFILE_END();
        return conversionCache;
    }
}

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_convert_depth_to_world(sensekit_depthstream_t depthStream,
                                                                  float depthX, float depthY, float depthZ,
                                                                  float* pWorldX, float* pWorldY, float* pWorldZ)
{
    PROFILE_FUNC();
    conversion_cache_t conversionCache = sensekit_depth_fetch_conversion_cache(depthStream);

    PROFILE_BEGIN(depth_to_world_math);
    float normalizedX = depthX / conversionCache.resolutionX - .5f;
    float normalizedY = .5f - depthY / conversionCache.resolutionY;

    *pWorldX = normalizedX * depthZ * conversionCache.xzFactor;
    *pWorldY = normalizedY * depthZ * conversionCache.yzFactor;
    *pWorldZ = depthZ;

    PROFILE_END();

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_convert_world_to_depth(sensekit_depthstream_t depthStream,
                                                                  float worldX, float worldY, float worldZ,
                                                                  float* pDepthX, float* pDepthY, float* pDepthZ)
{
    PROFILE_FUNC();
    conversion_cache_t conversionCache = sensekit_depth_fetch_conversion_cache(depthStream);

    *pDepthX = conversionCache.coeffX * worldX / worldZ + conversionCache.halfResX;
    *pDepthY = conversionCache.halfResY - conversionCache.coeffY * worldY / worldZ;
    *pDepthZ = worldZ;

    return SENSEKIT_STATUS_SUCCESS;
}


SENSEKIT_API_EX sensekit_status_t sensekit_reader_get_depthstream(sensekit_reader_t reader,
                                                                  sensekit_depthstream_t* depthStream)

{
    return sensekit_reader_get_stream(reader,
                                      SENSEKIT_STREAM_DEPTH,
                                      DEFAULT_SUBTYPE,
                                      depthStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthstream_get_depth_to_world_data(sensekit_depthstream_t depthStream,
                                                                               conversion_cache_t* conversion_data)
{
    conversion_cache_t conversionCache = sensekit_depth_fetch_conversion_cache(depthStream);
    memcpy(conversion_data, &conversionCache, sizeof(conversion_cache_t));
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthstream_get_hfov(sensekit_depthstream_t depthStream,
                                                                float* hFov)
{
    return sensekit_stream_get_parameter_fixed(depthStream,
                                               SENSEKIT_PARAMETER_IMAGE_HFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(hFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthstream_get_vfov(sensekit_depthstream_t depthStream,
                                                                float* vFov)
{
    return sensekit_stream_get_parameter_fixed(depthStream,
                                               SENSEKIT_PARAMETER_IMAGE_VFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(vFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_depthframe(sensekit_reader_frame_t readerFrame,
                                                                sensekit_depthframe_t* depthFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_DEPTH,
                                          DEFAULT_SUBTYPE,
                                          depthFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_frame_get_depthframe_with_subtype(sensekit_reader_frame_t readerFrame,
                                                                             sensekit_stream_subtype_t subtype,
                                                                             sensekit_depthframe_t* colorFrame)
{
    return sensekit_reader_get_imageframe(readerFrame,
                                          SENSEKIT_STREAM_DEPTH,
                                          subtype,
                                          colorFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_frameindex(sensekit_depthframe_t depthFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_generic_frame_get_frameindex(depthFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_byte_length(sensekit_depthframe_t depthFrame,
                                                                           size_t* byteLength)
{
    return sensekit_imageframe_get_data_byte_length(depthFrame, byteLength);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_ptr(sensekit_depthframe_t depthFrame,
                                                                   int16_t** data,
                                                                   size_t* byteLength)
{
    void* voidData = nullptr;
    sensekit_imageframe_get_data_ptr(depthFrame, &voidData, byteLength);
    *data = static_cast<int16_t*>(voidData);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_copy_data(sensekit_depthframe_t depthFrame,
                                                                int16_t* data)
{
    return sensekit_imageframe_copy_data(depthFrame, data);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_metadata(sensekit_depthframe_t depthFrame,
                                                                   sensekit_image_metadata_t* metadata)
{
    return sensekit_imageframe_get_metadata(depthFrame, metadata);
}

SENSEKIT_END_DECLS
