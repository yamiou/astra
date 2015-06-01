#include "PointProcessor.h"
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace xs {

    PointProcessor::PointProcessor(PluginServiceProxy& pluginService,
                   Sensor streamset,
                   StreamDescription& depthDesc,
                   PluginLogger& pluginLogger) :
       m_pluginService(pluginService),
       m_streamset(streamset),
       m_logger(pluginLogger),
       m_reader(streamset.create_reader())
    {
        PROFILE_FUNC();

        m_depthStream = m_reader.stream<DepthStream>(depthDesc.get_subtype());
        m_depthStream.start();

        m_reader.addListener(*this);
    }

    PointProcessor::~PointProcessor()
    {
        PROFILE_FUNC();

    }

    void PointProcessor::on_frame_ready(StreamReader& reader, Frame& frame)
    {
        PROFILE_FUNC();

        DepthFrame depthFrame = frame.get<DepthFrame>();

        create_point_stream_if_necessary(depthFrame);

        if (m_pointStream->has_connections())
        {
            process_depth(depthFrame);
        }
    }

    void PointProcessor::create_point_stream_if_necessary(DepthFrame& depthFrame)
    {
        PROFILE_FUNC();
        if (m_pointStream != nullptr)
        {
            return;
        }
        //TODO check for changes in depthFrame width and height and update bin size
        m_logger.info("creating point stream");

        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();
        m_pointStream = std::make_unique<PointStream>(m_pluginService, m_streamset, width, height);
    }

    void PointProcessor::process_depth(DepthFrame& depthFrame)
    {

    }

} } }
