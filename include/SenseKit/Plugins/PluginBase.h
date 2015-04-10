#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include <sensekit_types.h>
#include "PluginServiceProxy.h"

namespace sensekit {

    class PluginBase
    {
    public:
        PluginBase(PluginServiceProxy* pluginService)
            :  m_pluginService(pluginService)
            {}

        virtual ~PluginBase() = default;

        void initialize() { on_initialize(); };

        virtual void temp_update() { };

    protected:
        inline PluginServiceProxy& get_pluginService() const  { return *m_pluginService; }
        virtual void on_initialize() { };
    private:
        PluginServiceProxy* m_pluginService;

    };
}

#define EXPORT_PLUGIN(className)                                                          \
                                                                                          \
static className* g_plugin;                                                               \
                                                                                          \
SENSEKIT_BEGIN_DECLS                                                                      \
                                                                                          \
SENSEKIT_EXPORT void sensekit_plugin_initialize(PluginServiceProxyBase* pluginProxy)      \
{                                                                                         \
    g_plugin = new className(                                                             \
        static_cast<sensekit::PluginServiceProxy*>(pluginProxy));                         \
    g_plugin->initialize();                                                               \
}                                                                                         \
                                                                                          \
SENSEKIT_EXPORT void sensekit_plugin_update()                                             \
{                                                                                         \
    g_plugin->temp_update();                                                              \
}                                                                                         \
                                                                                          \
SENSEKIT_EXPORT void sensekit_plugin_terminate()                                          \
{                                                                                         \
    delete g_plugin;                                                                      \
    g_plugin = nullptr;                                                                   \
}                                                                                         \
                                                                                          \
SENSEKIT_END_DECLS                                                                        \
                                                                                          \
struct missing_semicolon ## __LINE__ {}

#endif /* PLUGINBASE_H */