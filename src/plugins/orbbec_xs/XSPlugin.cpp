#include "XSPlugin.h"
#include <Astra/Astra.h>
#include <Shiny.h>

EXPORT_PLUGIN(astra::plugins::xs::XSPlugin);

namespace astra { namespace plugins { namespace xs {

    void XSPlugin::on_stream_added(astra_streamset_t setHandle,
                                   astra_stream_t streamHandle,
                                   astra_stream_desc_t streamDesc)
    {
        PROFILE_FUNC();
        if (streamDesc.type == ASTRA_STREAM_DEPTH &&
            m_pointProcessorMap.find(streamHandle) == m_pointProcessorMap.end())
        {
            LOG_INFO("XSPlugin", "XS creating point processor");

            StreamDescription depthDescription = streamDesc;

            auto pointProcessorPtr = std::make_unique<PointProcessor>(pluginService(),
                                                                      setHandle,
                                                                      depthDescription);

            m_pointProcessorMap[streamHandle] = std::move(pointProcessorPtr);
        }
    }

    void XSPlugin::on_stream_removed(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t streamDesc)
    {
        PROFILE_FUNC();
        auto it = m_pointProcessorMap.find(streamHandle);
        if (it != m_pointProcessorMap.end())
        {
            m_pointProcessorMap.erase(it);
        }
    }

}}}
