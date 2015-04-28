#ifndef SCALINGCOORDINATEMAPPER_H
#define SCALINGCOORDINATEMAPPER_H

#include <opencv2/core/core.hpp>
#include <SenseKitUL/streams/Depth.h>

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
                                float scale)
            : m_scale(scale),
              m_mapper(mapper)
        { }

        float scale() const { return m_scale; }
        void set_scale(float scale) { m_scale = scale; }

        inline cv::Point3f convert_depth_to_world(int depthX, int depthY, float depthZ) const
        {
            depthX *= m_scale;
            depthY *= m_scale;

            return cv_convert_depth_to_world(m_mapper, depthX, depthY, depthZ);
        }

        inline cv::Point3f convert_depth_to_world(float depthX, float depthY, float depthZ) const
        {
            depthX *= m_scale;
            depthY *= m_scale;

            return cv_convert_depth_to_world(m_mapper, depthX, depthY, depthZ);
        }

        inline cv::Point3f convert_depth_to_world(cv::Point3f depthPosition) const
        {
            return convert_depth_to_world(depthPosition.x, depthPosition.y, depthPosition.z);
        }

        inline cv::Point3f convert_world_to_depth(cv::Point3f worldPosition) const
        {
            cv::Point3f depth = cv_convert_world_to_depth(m_mapper, worldPosition);

            depth.x /= m_scale;
            depth.y /= m_scale;

            return depth;
        }

    private:
        const sensekit::CoordinateMapper m_mapper;
        float m_scale;
    };

}}}

#endif // SCALINGCOORDINATEMAPPER_H