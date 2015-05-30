#include "XSPlugin.h"
#include <SenseKit/SenseKit.h>
#include <Shiny.h>

EXPORT_PLUGIN(sensekit::plugins::xs::XSPlugin);

namespace sensekit { namespace plugins { namespace xs {

    void XSPlugin::on_stream_added(sensekit_streamset_t setHandle,
                                   sensekit_stream_t streamHandle,
                                   sensekit_stream_desc_t streamDesc)
    {
        PROFILE_FUNC();
        get_logger().info("XS on stream added: %d,%d", streamDesc.type, streamDesc.subtype);
        if (streamDesc.type == SENSEKIT_STREAM_DEPTH &&
            m_pointProcessorMap.find(streamHandle) == m_pointProcessorMap.end())
        {
            get_logger().info("XS creating point processor");

            StreamDescription depthDescription = streamDesc;

            auto pointProcessorPtr = std::make_unique<PointProcessor>(get_pluginService(),
                                                                      setHandle,
                                                                      depthDescription,
                                                                      get_logger());

            m_pointProcessorMap[streamHandle] = std::move(pointProcessorPtr);
        }
    }

    void XSPlugin::on_stream_removed(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t streamDesc)
    {
        PROFILE_FUNC();
        get_logger().info("XS on stream removed: %d,%d", streamDesc.type, streamDesc.subtype);
        auto it = m_pointProcessorMap.find(streamHandle);
        if (it != m_pointProcessorMap.end())
        {
            m_pointProcessorMap.erase(it);
        }
    }

}}}
