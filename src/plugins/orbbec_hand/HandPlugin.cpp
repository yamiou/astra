#include "HandPlugin.h"
#include "HandTracker.h"

EXPORT_PLUGIN(sensekit::plugins::hand::HandPlugin);

namespace sensekit { namespace plugins { namespace hand {

    HandPlugin::HandPlugin(PluginServiceProxy* pluginProxy)
        : PluginBase(pluginProxy)
    {}

    HandPlugin::~HandPlugin()
    {}

    void HandPlugin::on_initialize()
    {
        get_pluginService().register_stream_added_callback(&HandPlugin::stream_added_handler_thunk,
                                                           this,
                                                           &m_streamAddedCallbackId);

        get_pluginService().register_stream_removing_callback(&HandPlugin::stream_removing_handler_thunk,
                                                              this,
                                                              &m_streamRemovingCallbackId);
    }

    void HandPlugin::stream_added_handler_thunk(void* clientTag,
                                                 sensekit_streamset_t setHandle,
                                                 sensekit_stream_t streamHandle,
                                                 sensekit_stream_desc_t desc)
    {
        HandPlugin* plugin = static_cast<HandPlugin*>(clientTag);
        plugin->stream_added_handler(setHandle, streamHandle, desc);
    }

    void HandPlugin::stream_removing_handler_thunk(void* clientTag,
                                                    sensekit_streamset_t setHandle,
                                                    sensekit_stream_t streamHandle,
                                                    sensekit_stream_desc_t desc)

    {
        HandPlugin* plugin = static_cast<HandPlugin*>(clientTag);
        plugin->stream_removing_handler(setHandle, streamHandle, desc);
    }

    void HandPlugin::stream_added_handler(sensekit_streamset_t setHandle,
                                           sensekit_stream_t streamHandle,
                                           sensekit_stream_desc_t streamDesc)
    {
        if (streamDesc.type == SENSEKIT_STREAM_DEPTH &&
            m_streamTrackerMap.find(streamHandle) == m_streamTrackerMap.end())
        {
            Sensor sensor(setHandle);
            StreamDescription depthDescription = streamDesc;
            HandTracker* tracker = new HandTracker(get_pluginService(), sensor, depthDescription);
            m_streamTrackerMap[streamHandle] = tracker;
        }
    }

    void HandPlugin::stream_removing_handler(sensekit_streamset_t setHandle,
                                              sensekit_stream_t streamHandle,
                                              sensekit_stream_desc_t desc)
    {
        auto it = m_streamTrackerMap.find(streamHandle);
        if (it != m_streamTrackerMap.end())
        {
            HandTracker* tracker = it->second;

            delete tracker;
            m_streamTrackerMap.erase(it);
        }
    }

}}}