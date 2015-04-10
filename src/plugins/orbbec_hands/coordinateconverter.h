#ifndef COORDINATECONVERTER_H
#define COORDINATECONVERTER_H
#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/depth.h>

class CoordinateConverter
{
private:
    float m_resizeFactor;
public:
    CoordinateConverter(float resizeFactor) :
        m_resizeFactor(resizeFactor)
    { }

    float get_resizeFactor() const { return m_resizeFactor; }
    void set_resizeFactor(float resizeFactor) { m_resizeFactor = resizeFactor; }

    inline cv::Point3f convertDepthToRealWorld(int localX, int localY, float localZ) const
    {
        return convertDepthToRealWorld(static_cast<float>(localX), static_cast<float>(localY), localZ);
    }

    inline cv::Point3f convertDepthToRealWorld(float localX, float localY, float localZ) const
    {
        float worldX, worldY, worldZ;

        localX *= m_resizeFactor;
        localY *= m_resizeFactor;

        convert_depth_to_world(localX, localY, localZ, &worldX, &worldY, &worldZ);

        return cv::Point3f(worldX, worldY, worldZ);
    }

    inline cv::Point3f convertDepthToRealWorld(cv::Point3f localPosition) const
    {
        return convertDepthToRealWorld(localPosition.x, localPosition.y, localPosition.z);
    }

    inline cv::Point3f convertRealWorldToDepth(cv::Point3f worldPosition) const
    {
        float localX, localY, localZ;

        convert_world_to_depth(worldPosition.x, worldPosition.y, worldPosition.z, &localX, &localY, &localZ);

        localX /= m_resizeFactor;
        localY /= m_resizeFactor;

        return cv::Point3f(localX, localY, localZ);
    }

    inline cv::Point offsetPixelLocationByMM(cv::Point& position, float offsetX, float offsetY, float depth) const
    {
        cv::Point3f world = convertDepthToRealWorld(static_cast<float>(position.x), static_cast<float>(position.y), depth);

        world.x += offsetX;
        world.y += offsetY;

        cv::Point3f offsetLocal = convertRealWorldToDepth(world);

        return cv::Point(static_cast<int>(offsetLocal.x), static_cast<int>(offsetLocal.y));
    }
};

#endif // COORDINATECONVERTER_H