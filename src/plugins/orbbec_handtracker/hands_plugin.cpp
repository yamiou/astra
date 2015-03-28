#include "hands_plugin.h"
#include <iostream>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

static HandTrackerPlugin* g_plugin;

SENSEKIT_EXPORT void sensekit_plugin_initialize(StreamServiceProxyBase* streamProxy,
                                                    PluginServiceProxyBase* pluginProxy)
{
    g_plugin = new HandTrackerPlugin(
        static_cast<sensekit::StreamServiceProxy*>(streamProxy),
        static_cast<sensekit::PluginServiceProxy*>(pluginProxy));

    g_plugin->initialize();
}

SENSEKIT_EXPORT void sensekit_plugin_update()
{
    g_plugin->temp_update();
}

SENSEKIT_EXPORT void sensekit_plugin_terminate()
{
    g_plugin->cleanup();
    delete g_plugin;
}

SENSEKIT_END_DECLS