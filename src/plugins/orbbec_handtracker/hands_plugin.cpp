#include "hands_plugin.h"
#include <iostream>
#include <StreamTypes.h>
//#include <streams/hands_types.h>
//
//SENSEKIT_BEGIN_DECLS
//
//SENSEKIT_API_EXPORT void sensekit_plugin_initialize(StreamServiceProxyBase* streamProxy,
//PluginServiceProxyBase* pluginProxy)
//{
//    g_plugin = new sensekit::openni::OpenNIPlugin(
//        static_cast<sensekit::StreamServiceProxy*>(streamProxy),
//        static_cast<sensekit::PluginServiceProxy*>(pluginProxy));
//
//    g_plugin->initialize();
//}
//
//SENSEKIT_API_EXPORT void sensekit_plugin_update()
//{
//    g_plugin->temp_update();
//}
//
//SENSEKIT_API_EXPORT void sensekit_plugin_terminate()
//{
//    g_plugin->cleanup();
//    delete g_plugin;
//}
//
//SENSEKIT_END_DECLS