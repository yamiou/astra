#include "DepthPlugin.h"
#include <SenseKit/SenseKit.h>

EXPORT_PLUGIN(sensekit::plugins::depth::DepthPlugin);

namespace sensekit { namespace plugins { namespace depth {

    void DepthPlugin::on_stream_added(sensekit_streamset_t setHandle,
                                      sensekit_stream_t streamHandle,
                                      sensekit_stream_desc_t desc)
    {
        if (desc.type != SENSEKIT_STREAM_DEPTH)
            return; // if new stream is not depth, we don't care.

        m_depthStreams.push_back(std::make_unique<StylizedDepthStream>(get_pluginService(),
                                                                       Sensor(setHandle),
                                                                       streamHandle));
    }

    void DepthPlugin::on_stream_removed(sensekit_streamset_t setHandle,
                                        sensekit_stream_t streamHandle,
                                        sensekit_stream_desc_t desc)
    {
        auto it = std::find_if(m_depthStreams.cbegin(),
                               m_depthStreams.cend(),
                               [&streamHandle] (const DepthStreamPtr& depthStreamPtr)
                               {
                                   return depthStreamPtr->get_handle() == streamHandle;
                               });

        if (it != m_depthStreams.cend())
        {
            m_depthStreams.erase(it);
        }
    }

}}}
