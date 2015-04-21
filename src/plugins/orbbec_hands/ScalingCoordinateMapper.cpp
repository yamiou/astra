#include "ScalingCoordinateMapper.h"
#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/Depth.h>

namespace sensekit { namespace plugins { namespace hands {

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          float depthX, float depthY, float depthZ)
    {
        float worldX, worldY, worldZ;

        mapper.convert_depth_to_world(depthX, depthY, depthZ, &worldX, &worldY, &worldZ);

        return cv::Point3f(worldX, worldY, worldZ);
    }

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          int depthX, int depthY, float depthZ)
    {
        return cv_convert_depth_to_world(mapper,
                                         static_cast<float>(depthX),
                                         static_cast<float>(depthY),
                                         depthZ);
    }

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          cv::Point3f depth)
    {
        float worldX, worldY, worldZ;

        return cv_convert_depth_to_world(mapper, depth.x, depth.y, depth.z);
    }

    cv::Point3f cv_convert_world_to_depth(const sensekit::CoordinateMapper& mapper,
                                          float worldX, float worldY, float worldZ)
    {
        float depthX, depthY, depthZ;
        mapper.convert_world_to_depth(worldX,
                                      worldY,
                                      worldZ,
                                      &depthX,
                                      &depthY,
                                      &depthZ);

        return cv::Point3f(depthX, depthY, depthZ);
    }

    cv::Point3f cv_convert_world_to_depth(const sensekit::CoordinateMapper& mapper,
                                          cv::Point3f world)
    {
        return cv_convert_world_to_depth(mapper, world.x, world.y, world.z);
    }


    cv::Point offset_pixel_location_by_mm(const ScalingCoordinateMapper& mapper,
                                          cv::Point& position,
                                          float offsetX,
                                          float offsetY,
                                          float depth)
    {
        cv::Point3f world = mapper.convert_depth_to_world(static_cast<float>(position.x),
                                                           static_cast<float>(position.y),
                                                           depth);

        world.x += offsetX;
        world.y += offsetY;

        cv::Point3f offsetLocal = mapper.convert_world_to_depth(world);

        return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
    }
}}}
