#include "xs_plugin.hpp"
#include <astra_core/astra_core.hpp>
#include <Shiny.h>

EXPORT_PLUGIN(astra::xs::plugin);

namespace astra { namespace xs {

    void plugin::on_stream_added(astra_streamset_t setHandle,
                                 astra_stream_t streamHandle,
                                 astra_stream_desc_t streamDesc)
    {
        if (streamDesc.type == ASTRA_STREAM_DEPTH &&
            pointProcessorMap_.find(streamHandle) == pointProcessorMap_.end())
        {
            LOG_INFO("astra.xs.plugin", "creating point processor");

            stream_description depthDescription = streamDesc;

            auto ppPtr = std::make_unique<point_processor>(pluginService(),
                                                           setHandle,
                                                           depthDescription);

            pointProcessorMap_[streamHandle] = std::move(ppPtr);
        }
    }

    void plugin::on_stream_removed(astra_streamset_t setHandle,
                                   astra_stream_t streamHandle,
                                   astra_stream_desc_t streamDesc)
    {
        auto it = pointProcessorMap_.find(streamHandle);
        if (it != pointProcessorMap_.end())
        {
            pointProcessorMap_.erase(it);
        }
    }

}}
