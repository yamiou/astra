#ifndef COORDINATECONVERSION_H
#define COORDINATECONVERSION_H
#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/depth.h>

class CoordinateConversion
{
public:
    static inline cv::Point3f convertDepthToRealWorld(float localX, float localY, float localZ, const float resizeFactor)
    {
        float worldX, worldY, worldZ;

        localX *= resizeFactor;
        localY *= resizeFactor;

        convert_depth_to_world(localX, localY, localZ, &worldX, &worldY, &worldZ);

        return cv::Point3f(worldX, worldY, worldZ);
    }

    static inline cv::Point3f convertDepthToRealWorld(cv::Point3f localPosition, const float resizeFactor)
    {
        return convertDepthToRealWorld(localPosition.x, localPosition.y, localPosition.z, resizeFactor);
    }

    static inline cv::Point3f convertRealWorldToDepth(cv::Point3f worldPosition, const float resizeFactor)
    {
        float localX, localY, localZ;

        convert_world_to_depth(worldPosition.x, worldPosition.y, worldPosition.z, &localX, &localY, &localZ);

        localX /= resizeFactor;
        localY /= resizeFactor;

        return cv::Point3f(localX, localY, localZ);
    }

    static inline cv::Point offsetPixelLocationByMM(cv::Point& position, float offsetX, float offsetY, float depth, const float resizeFactor)
    {
        cv::Point3f world = convertDepthToRealWorld(position.x, position.y, depth, resizeFactor);

        world.x += offsetX;
        world.y += offsetY;

        cv::Point3f offsetLocal = convertRealWorldToDepth(world, resizeFactor);

        return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
    }
};

#endif // COORDINATECONVERSION_H