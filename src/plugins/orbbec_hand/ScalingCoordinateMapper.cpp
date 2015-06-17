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


    cv::Point offset_pixel_location_by_mm(const ScalingCoordinateMapper& mapper,
                                          const cv::Point& position,
                                          float offsetX,
                                          float offsetY,
                                          float depth)
    {
        PROFILE_FUNC();
        if (depth == 0)
        {
            return position;
        }
        cv::Point3f world = mapper.convert_depth_to_world(static_cast<float>(position.x),
                                                          static_cast<float>(position.y),
                                                          depth);

        world.x += offsetX;
        world.y += offsetY;

        cv::Point3f offsetLocal = mapper.convert_world_to_depth(world);

        return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
    }
}}}
