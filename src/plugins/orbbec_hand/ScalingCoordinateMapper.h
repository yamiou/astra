#ifndef SCALINGCOORDINATEMAPPER_H
#define SCALINGCOORDINATEMAPPER_H

#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/Depth.h>
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace hand {

    class ScalingCoordinateMapper;

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          int depthX, int depthY, float depthZ);

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          float depthX, float depthY, float depthZ);

    cv::Point3f cv_convert_depth_to_world(const sensekit::CoordinateMapper& mapper,
                                          const cv::Point3f& depth);

    cv::Point3f cv_convert_world_to_depth(const sensekit::CoordinateMapper& mapper,
                                          float worldX, float worldY, float worldZ);

    cv::Point3f cv_convert_world_to_depth(const sensekit::CoordinateMapper& mapper,
                                          const cv::Point3f& world);

    cv::Point offset_pixel_location_by_mm(const ScalingCoordinateMapper& mapper,
                                          const cv::Point& position,
                                          float offsetX,
                                          float offsetY,
                                          float depth);

    class ScalingCoordinateMapper
    {
    public:
        ScalingCoordinateMapper(const sensekit::CoordinateMapper& mapper,
                                const float scale,
                                const float offsetX = 0,
                                const float offsetY = 0)
            : m_mapper(mapper),
              m_scale(scale),
              m_offsetX(offsetX),
              m_offsetY(offsetY)
        { }

        float scale() const { return m_scale; }

        inline cv::Point3f convert_depth_to_world(int depthX, int depthY, float depthZ) const
        {
            PROFILE_FUNC();
            depthX = (depthX + m_offsetX) * m_scale;
            depthY = (depthY + m_offsetY) * m_scale;

            return cv_convert_depth_to_world(m_mapper, depthX, depthY, depthZ);
        }

        inline void convert_depth_to_world(int depthX, int depthY, float depthZ,
                                           float& worldX, float& worldY, float& worldZ) const
        {
            PROFILE_FUNC();
            depthX = (depthX + m_offsetX) * m_scale;
            depthY = (depthY + m_offsetY) * m_scale;

            m_mapper.convert_depth_to_world(depthX, depthY, depthZ, &worldX, &worldY, &worldZ);
        }

        inline cv::Point3f convert_depth_to_world(float depthX, float depthY, float depthZ) const
        {
            PROFILE_FUNC();
            depthX = (depthX + m_offsetX) * m_scale;
            depthY = (depthY + m_offsetY) * m_scale;

            return cv_convert_depth_to_world(m_mapper, depthX, depthY, depthZ);
        }

        inline cv::Point3f convert_depth_to_world(cv::Point3f depthPosition) const
        {
            PROFILE_FUNC();
            return convert_depth_to_world(depthPosition.x, depthPosition.y, depthPosition.z);
        }

        inline cv::Point3f convert_world_to_depth(cv::Point3f worldPosition) const
        {
            PROFILE_FUNC();
            cv::Point3f depth = cv_convert_world_to_depth(m_mapper, worldPosition);

            depth.x = (depth.x / m_scale) - m_offsetX;
            depth.y = (depth.y / m_scale) - m_offsetX;

            return depth;
        }

    private:
        const sensekit::CoordinateMapper m_mapper;
        const float m_scale;
        const float m_offsetX;
        const float m_offsetY;
    };

}}}

#endif // SCALINGCOORDINATEMAPPER_H
