#include "xs_point_processor.hpp"
#include <Shiny.h>

namespace astra { namespace xs {

    point_processor::point_processor(PluginServiceProxy& pluginService,
                                     astra_streamset_t streamset,
                                     stream_description& depthDesc)
        : streamset_(get_uri_for_streamset(pluginService, streamset)),
          setHandle_(streamset),
          reader_(streamset_.create_reader()),
          depthStream_(reader_.stream<depthstream>(depthDesc.subtype())),
          pluginService_(pluginService)
    {
        depthStream_.start();
        reader_.addListener(*this);
    }

    point_processor::~point_processor() = default;

    void point_processor::on_frame_ready(stream_reader& reader, frame& frame)
    {
        depthframe depthFrame = frame.get<depthframe>();

        create_point_stream_if_necessary(depthFrame);

        if (pointStream_->has_connections())
        {
            LOG_TRACE("astra.xs.point_processor", "updating point frame");
            update_pointframe_from_depth(depthFrame);
        }
    }

    void point_processor::create_point_stream_if_necessary(depthframe& depthFrame)
    {
        if (pointStream_ != nullptr)
        {
            return;
        }

        //TODO check for changes in depthFrame width and height and update bin size
        LOG_INFO("astra.xs.point_processor", "creating point stream");

        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();

        auto ps = plugins::make_stream<pointstream>(pluginService_, setHandle_, width, height);
        pointStream_ = std::unique_ptr<pointstream>(std::move(ps));

        LOG_INFO("astra.xs.point_processor", "created point stream");

        depthConversionCache_ = depthStream_.depth_to_world_data();
    }

    void point_processor::update_pointframe_from_depth(depthframe& depthFrame)
    {
        //use same frameIndex as source depth frame
        astra_frame_index_t frameIndex = depthFrame.frameIndex();

        astra_imageframe_wrapper_t* pointFrameWrapper = pointStream_->begin_write(frameIndex);

        if (pointFrameWrapper != nullptr)
        {
            pointFrameWrapper->frame.frame = nullptr;
            pointFrameWrapper->frame.data = &pointFrameWrapper->frame_data[0];

            astra_image_metadata_t metadata;

            metadata.width = depthFrame.resolutionX();
            metadata.height = depthFrame.resolutionY();
            metadata.pixelFormat = ASTRA_PIXEL_FORMAT_POINT;

            pointFrameWrapper->frame.metadata = metadata;

            vector3f* p_points = reinterpret_cast<vector3f*>(pointFrameWrapper->frame.data);
            calculate_point_frame(depthFrame, p_points);

            pointStream_->end_write();
        }
    }

    void point_processor::calculate_point_frame(depthframe& depthFrame,
                                                vector3f* p_points)
    {
        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();
        const int16_t* p_depth = depthFrame.data();

        const conversion_cache_t conversionData = depthConversionCache_;

        for (int y = 0; y < height; ++y)
        {
            for (int x = 0; x < width; ++x, ++p_points, ++p_depth)
            {
                uint16_t depth = *p_depth;
                vector3f& point = *p_points;

                float normalizedX = static_cast<float>(x) / conversionData.resolutionX - .5f;
                float normalizedY = .5f - static_cast<float>(y) / conversionData.resolutionY;

                point.x = normalizedX * depth * conversionData.xzFactor;
                point.y = normalizedY * depth * conversionData.yzFactor;
                point.z = depth;
            }
        }
    }
}}
