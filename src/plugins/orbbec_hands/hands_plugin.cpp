#include "handtracker.h"
#include "hands_plugin.h"

EXPORT_PLUGIN(sensekit::hands::HandsPlugin);

namespace sensekit
{
    namespace hands
    {
        HandsPlugin::HandsPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy)
        { }

        HandsPlugin::~HandsPlugin()
        {
        }

        void HandsPlugin::on_initialize()
        {
            get_pluginService().register_stream_added_callback(&HandsPlugin::stream_added_handler_thunk, this, &m_streamAddedCallbackId);
            get_pluginService().register_stream_removing_callback(&HandsPlugin::stream_removing_handler_thunk, this, &m_streamRemovingCallbackId);
        }

        void HandsPlugin::stream_added_handler_thunk(void* clientTag,
                                                     sensekit_streamset_t setHandle,
                                                     sensekit_stream_t streamHandle,
                                                     sensekit_stream_desc_t desc)
        {
            HandsPlugin* plugin = static_cast<HandsPlugin*>(clientTag);
            plugin->stream_added_handler(setHandle, streamHandle, desc);
        }

        void HandsPlugin::stream_removing_handler_thunk(void* clientTag,
                                                        sensekit_streamset_t setHandle,
                                                        sensekit_stream_t streamHandle,
                                                        sensekit_stream_desc_t desc)

        {
            HandsPlugin* plugin = static_cast<HandsPlugin*>(clientTag);
            plugin->stream_removing_handler(setHandle, streamHandle, desc);
        }

        void HandsPlugin::stream_added_handler(sensekit_streamset_t setHandle,
                                               sensekit_stream_t streamHandle,
                                               sensekit_stream_desc_t streamDesc)
        {
            if (streamDesc.type == SENSEKIT_STREAM_DEPTH &&
                m_streamTrackerMap.find(streamHandle) == m_streamTrackerMap.end())
            {
                HandTracker* tracker = new HandTracker(&get_pluginService());
                tracker->setupStream(setHandle, streamDesc);
                m_streamTrackerMap[streamHandle] = tracker;
            }
        }

        void HandsPlugin::stream_removing_handler(sensekit_streamset_t setHandle,
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
    }
}