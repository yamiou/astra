#include "handtracker.h"
#include "hands_plugin.h"

EXPORT_PLUGIN(sensekit::hands::HandsPlugin);

namespace sensekit
{
    namespace hands
    {
        HandsPlugin::HandsPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy)
        {
            get_pluginService().register_stream_added_callback(&HandsPlugin::stream_added_handler_thunk, &m_streamAddedCallbackId);
            get_pluginService().register_stream_removing_callback(&HandsPlugin::stream_removing_handler_thunk, &m_streamRemovingCallbackId);
        }

        HandsPlugin::~HandsPlugin()
        {
        }

        void HandsPlugin::stream_added_handler_thunk(sensekit_streamset_t setHandle,
                                                     sensekit_stream_t streamHandle,
                                                     sensekit_stream_desc_t desc)
        {
            g_plugin->stream_added_handler(setHandle, streamHandle, desc);
        }

        void HandsPlugin::stream_removing_handler_thunk(sensekit_streamset_t setHandle,
                                                        sensekit_stream_t streamHandle,
                                                        sensekit_stream_desc_t desc)

        {
            g_plugin->stream_removing_handler(setHandle, streamHandle, desc);
        }

        void HandsPlugin::stream_added_handler(sensekit_streamset_t setHandle,
                                               sensekit_stream_t streamHandle,
                                               sensekit_stream_desc_t desc)
        {
            if (desc.type == SENSEKIT_STREAM_DEPTH &&
                m_streamTrackerMap.find(streamHandle) == m_streamTrackerMap.end())
            {
                sensekit_depthstream_t* depthStream = reinterpret_cast<sensekit_depthstream_t*>(streamHandle);

                HandTracker* tracker = new HandTracker(&get_pluginService(), setHandle, depthStream);
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