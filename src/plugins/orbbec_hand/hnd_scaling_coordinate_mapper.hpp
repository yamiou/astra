#ifndef HND_SCALING_COORDINATE_MAPPER_H
#define HND_SCALING_COORDINATE_MAPPER_H

#include <opencv2/core/core.hpp>
#include <AstraUL/streams/Depth.h>
#include <Shiny.h>

namespace astra { namespace hand {

    class scaling_coordinate_mapper;

    void convert_depth_to_world_f(const conversion_cache_t& depthToWorldData,
                                  float depthX, float depthY, float depthZ,
                                  float& worldX, float& worldY, float& worldZ);

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          int depthX, int depthY, float depthZ);

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          float depthX, float depthY, float depthZ);

    cv::Point3f cv_convert_depth_to_world(const conversion_cache_t& depthToWorldData,
                                          const cv::Point3f& depth);

    cv::Point3f cv_convert_world_to_depth(const conversion_cache_t& depthToWorldData,
                                          float worldX, float worldY, float worldZ);

    cv::Point3f cv_convert_world_to_depth(const conversion_cache_t& depthToWorldData,
                                          const cv::Point3f& world);

    class scaling_coordinate_mapper
    {
    public:
        scaling_coordinate_mapper(const conversion_cache_t depthToWorldData,
                                  const float scale,
                                  const float offsetX = 0,
                                  const float offsetY = 0)
            : depthToWorldData_(depthToWorldData),
              scale_(scale),
              offsetX_(offsetX),
              offsetY_(offsetY)
        { }

        inline cv::Point3f convert_depth_to_world(int depthX, int depthY, float depthZ) const
        {
            PROFILE_FUNC();
            depthX = static_cast<int>((depthX + offsetX_) * scale_);
            depthY = static_cast<int>((depthY + offsetY_) * scale_);

            return cv_convert_depth_to_world(depthToWorldData_, depthX, depthY, depthZ);
        }

        inline void convert_depth_to_world(int depthX, int depthY, float depthZ,
                                           float& worldX, float& worldY, float& worldZ) const
        {
            PROFILE_FUNC();
            depthX = static_cast<int>((depthX + offsetX_) * scale_);
            depthY = static_cast<int>((depthY + offsetY_) * scale_);

            convert_depth_to_world_f(depthToWorldData_,
                                     depthX, depthY, static_cast<int>(depthZ),
                                     worldX, worldY, worldZ);
        }

        inline cv::Point3f convert_depth_to_world(float depthX, float depthY, float depthZ) const
        {
            PROFILE_FUNC();
            depthX = (depthX + offsetX_) * scale_;
            depthY = (depthY + offsetY_) * scale_;

            return cv_convert_depth_to_world(depthToWorldData_, depthX, depthY, depthZ);
        }

        inline cv::Point3f convert_depth_to_world(cv::Point3f depthPosition) const
        {
            PROFILE_FUNC();
            return convert_depth_to_world(depthPosition.x, depthPosition.y, depthPosition.z);
        }

        inline cv::Point3f convert_world_to_depth(cv::Point3f worldPosition) const
        {
            PROFILE_FUNC();
            cv::Point3f depth = cv_convert_world_to_depth(depthToWorldData_, worldPosition);

            depth.x = (depth.x / scale_) - offsetX_;
            depth.y = (depth.y / scale_) - offsetY_;

            return depth;
        }

        cv::Point offset_pixel_location_by_mm(const cv::Point& position,
                                              float offsetX,
                                              float offsetY,
                                              float depthZ) const;

        inline float scale() const { return scale_; }
        inline float offsetX() const { return offsetX_; }
        inline float offsetY() const { return offsetY_; }

    private:
        const conversion_cache_t depthToWorldData_;
        const float scale_;
        const float offsetX_;
        const float offsetY_;
    };
}}

#endif // HND_SCALING_COORDINATE_MAPPER_H
