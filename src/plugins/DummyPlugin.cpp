#include "DummyPlugin.h"
#include "SenseKit.h"
static sensekit::DummyPlugin* g_plugin;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EXPORT void sensekit_plugin_initialize(StreamServiceProxyBase* streamProxy,
                                                    PluginServiceProxyBase* pluginProxy)
{
    sensekitSL_initialize(reinterpret_cast<sensekit_context_t*>(streamProxy));
    g_plugin = new sensekit::DummyPlugin(
        static_cast<sensekit::StreamServiceProxy*>(streamProxy),
        static_cast<sensekit::PluginServiceProxy*>(pluginProxy));

    g_plugin->initialize();
}

SENSEKIT_API_EXPORT void sensekit_plugin_update()
{
    g_plugin->temp_update();
}

SENSEKIT_API_EXPORT void sensekit_plugin_terminate()
{
    g_plugin->cleanup();
    delete g_plugin;
}

SENSEKIT_END_DECLS
