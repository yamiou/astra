#include <SenseKit/sensekit_types.h>
#include "generic_stream_api.h"
#include <SenseKitUL/streams/depth_types.h>
#include <memory.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKitUL/streams/video_parameters.h>
#include <SenseKitUL/streams/depth_parameters.h>
#include <string.h>
#include <SenseKitUL/streams/image_types.h>
#include <SenseKitUL/streams/image_capi.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_convert_depth_to_world(sensekit_depthstream_t depthStream,
                                                                  float depthX, float depthY, float depthZ,
                                                                  float* pWorldX, float* pWorldY, float* pWorldZ)
{
    conversion_cache_t conversionCache;
    sensekit_stream_get_parameter_fixed(depthStream,
                                        DEPTH_PARAMETER_CONVERSION_CACHE,
                                        sizeof(conversion_cache_t),
                                        reinterpret_cast<sensekit_parameter_data_t*>(&conversionCache));

    float normalizedX = depthX / conversionCache.resolutionX - .5f;
    float normalizedY = .5f - depthY / conversionCache.resolutionY;

    *pWorldX = normalizedX * depthZ * conversionCache.xzFactor;
    *pWorldY = normalizedY * depthZ * conversionCache.yzFactor;
    *pWorldZ = depthZ;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API_EX sensekit_status_t sensekit_convert_world_to_depth(sensekit_depthstream_t depthStream,
                                                                  float worldX, float worldY, float worldZ,
                                                                  float* pDepthX, float* pDepthY, float* pDepthZ)
{
    conversion_cache_t conversionCache;
    sensekit_stream_get_parameter_fixed(depthStream,
                                        DEPTH_PARAMETER_CONVERSION_CACHE,
                                        sizeof(conversion_cache_t),
                                        reinterpret_cast<sensekit_parameter_data_t*>(&conversionCache));

    *pDepthX = conversionCache.coeffX * worldX / worldZ + conversionCache.halfResX;
    *pDepthY = conversionCache.halfResY - conversionCache.coeffY * worldY / worldZ;
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

SENSEKIT_API_EX sensekit_status_t sensekit_depth_stream_get_hfov(sensekit_depthstream_t depthStream,
                                                                 float* hFov)
{
    return sensekit_stream_get_parameter_fixed(depthStream,
                                               STREAM_PARAMETER_HFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(hFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_depth_stream_get_vfov(sensekit_depthstream_t depthStream,
                                                                 float* vFov)
{
    return sensekit_stream_get_parameter_fixed(depthStream,
                                               STREAM_PARAMETER_VFOV,
                                               sizeof(float),
                                               reinterpret_cast<sensekit_parameter_data_t*>(vFov));
}

SENSEKIT_API_EX sensekit_status_t sensekit_depth_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_depthframe_t* depthFrame)
{
    return sensekit_reader_get_imageframe(readerFrame, 
                                          SENSEKIT_STREAM_DEPTH,
                                          DEFAULT_SUBTYPE,
                                          depthFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_frameindex(sensekit_depthframe_t depthFrame,
                                                                     sensekit_frame_index_t* index)
{
    return sensekit_imageframe_get_frameindex(depthFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_length(sensekit_depthframe_t depthFrame,
                                                                      size_t* length)
{
    return sensekit_imageframe_get_data_byte_length(depthFrame, length);
}

SENSEKIT_API_EX sensekit_status_t sensekit_depthframe_get_data_ptr(sensekit_depthframe_t depthFrame,
                                                                   int16_t** data,
                                                                   size_t* length)
{
    void* voidData = nullptr;
    sensekit_imageframe_get_data_ptr(depthFrame, &voidData, length);
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
