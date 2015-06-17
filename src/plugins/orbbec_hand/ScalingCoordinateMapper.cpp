#include "ScalingCoordinateMapper.h"
#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/Depth.h>
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace hand {

    void convert_depth_to_world_f(const conversion_cache_t& depthToWorldData,
                                  float depthX, float depthY, float depthZ,
                                  float& worldX, float& worldY, float& worldZ)
    {
        float normalizedX = depthX / depthToWorldData.resolutionX - .5f;
        float normalizedY = .5f - depthY / depthToWorldData.resolutionY;

        worldX = normalizedX * depthZ * depthToWorldData.xzFactor;
        worldY = normalizedY * depthZ * depthToWorldData.yzFactor;
        worldZ = depthZ;
    }

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          float depthX, float depthY, float depthZ)
    {
        PROFILE_FUNC();
        float worldX, worldY, worldZ;

        convert_depth_to_world_f(depthToWorldData,
                                 depthX, depthY, depthZ,
                                 worldX, worldY, worldZ);

        return cv::Point3f(worldX, worldY, worldZ);
    }

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          int depthX, int depthY, float depthZ)
    {
        PROFILE_FUNC();
        return cv_convert_depth_to_world(depthToWorldData,
                                         static_cast<float>(depthX),
                                         static_cast<float>(depthY),
                                         depthZ);
    }

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          const cv::Point3f& depth)
    {
        PROFILE_FUNC();
        return cv_convert_depth_to_world(depthToWorldData, depth.x, depth.y, depth.z);
    }

    cv::Point3f cv_convert_world_to_depth(const conversion_cache_t& depthToWorldData,
                                          float worldX, float worldY, float worldZ)
    {
        PROFILE_FUNC();

        float depthX = depthToWorldData.coeffX * worldX / worldZ + depthToWorldData.halfResX;
        float depthY = depthToWorldData.halfResY - depthToWorldData.coeffY * worldY / worldZ;
        float depthZ = worldZ;

        return cv::Point3f(depthX, depthY, depthZ);
    }

    cv::Point3f cv_convert_world_to_depth(const conversion_cache_t& depthToWorldData,
                                          const cv::Point3f& world)
    {
        PROFILE_FUNC();
        return cv_convert_world_to_depth(depthToWorldData, world.x, world.y, world.z);
    }


    cv::Point ScalingCoordinateMapper::offset_pixel_location_by_mm(const cv::Point& position,
                                                                   float offsetX,
                                                                   float offsetY,
                                                                   float depthZ) const
    {
        PROFILE_FUNC();
        if (depthZ == 0)
        {
            return position;
        }

        const conversion_cache_t& depthToWorldData = m_depthToWorldData;

        float fullSizeDepthX = (position.x + m_offsetX) * m_scale;
        float fullSizeDepthY = (position.y + m_offsetY) * m_scale;

        float normalizedX = fullSizeDepthX / depthToWorldData.resolutionX - .5f;
        float normalizedY = .5f - fullSizeDepthY / depthToWorldData.resolutionY;

        float worldX = normalizedX * depthZ * depthToWorldData.xzFactor;
        float worldY = normalizedY * depthZ * depthToWorldData.yzFactor;

        worldX += offsetX;
        worldY += offsetY;

        float fullSizeDepthX2 = depthToWorldData.coeffX * worldX / depthZ + depthToWorldData.halfResX;
        float fullSizeDepthY2 = depthToWorldData.halfResY - depthToWorldData.coeffY * worldY / depthZ;

        float finalDepthX = (fullSizeDepthX2 / m_scale) - m_offsetX;
        float finalDepthY = (fullSizeDepthY2 / m_scale) - m_offsetY;

        return cv::Point(static_cast<int>(finalDepthX), static_cast<int>(finalDepthY));
    }
}}}
