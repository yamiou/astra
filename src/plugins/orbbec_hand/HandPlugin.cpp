#include "HandSettings.h"
#include "HandPlugin.h"
#include "HandTracker.h"
#include <Shiny.h>

EXPORT_PLUGIN(sensekit::plugins::hand::HandPlugin);

namespace sensekit { namespace plugins { namespace hand {

    const char HANDPLUGIN_CONFIG_FILE[] = "plugins/orbbec_hand.toml";

    HandPlugin::HandPlugin(PluginServiceProxy* pluginProxy)
        : PluginBase(pluginProxy, "orbbec_hand")
    {
        m_settings = parse_settings(HANDPLUGIN_CONFIG_FILE);
    }

    HandPlugin::~HandPlugin()
    {
        #ifndef __ANDROID__
            PROFILE_UPDATE();
            PROFILE_OUTPUT("profile_orbbec_hand.txt");
        #endif
    }

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
            const char* uri;
            get_pluginService().get_streamset_uri(setHandle, &uri);

            Sensor sensor(uri);
            StreamDescription depthDescription = streamDesc;

            HandTracker* tracker = new HandTracker(get_pluginService(),
                                                   setHandle,
                                                   depthDescription,
                                                   get_logger(),
                                                   m_settings);

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
