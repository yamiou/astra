#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include <sensekit_core.h>
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

        static void set_parameter_thunk(void* instance,
                                        sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data)
            {
                static_cast<PluginBase*>(instance)->set_parameter(connection, id, byteLength, data);
            }

        static void get_parameter_size_thunk(void* instance,
                                             sensekit_streamconnection_t connection,
                                             sensekit_parameter_id id,
                                             size_t* byteLength)
            {
                static_cast<PluginBase*>(instance)->get_parameter_size(connection, id, *byteLength);
            }

        static void get_parameter_data_thunk(void* instance,
                                             sensekit_streamconnection_t connection,
                                             sensekit_parameter_id id,
                                             size_t byteLength,
                                             sensekit_parameter_data_t* data)
            {
                static_cast<PluginBase*>(instance)->get_parameter_data(connection, id, byteLength, data);
            }

        static void connection_added_thunk(void* instance,
                                           sensekit_streamconnection_t connection)
            {
                static_cast<PluginBase*>(instance)->connection_added(connection);
            }

        static void connection_removed_thunk(void* instance,
                                             sensekit_streamconnection_t connection)
            {
                static_cast<PluginBase*>(instance)->connection_removed(connection);
            }

        virtual void set_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   size_t byteLength,
                                   sensekit_parameter_data_t* data) {}

        virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t& byteLength) {}

        virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data) {}

        virtual void connection_added(sensekit_streamconnection_t connection) {}
        virtual void connection_removed(sensekit_streamconnection_t connection) {}

        friend stream_callbacks_t create_plugin_callbacks(PluginBase* context);
    };

    inline stream_callbacks_t create_plugin_callbacks(PluginBase* context)
    {
        stream_callbacks_t callbacks;

        callbacks.context = context;
        callbacks.connectionAddedCallback = &PluginBase::connection_added_thunk;
        callbacks.connectionRemovedCallback = &PluginBase::connection_removed_thunk;
        callbacks.getParameterDataCallback = &PluginBase::get_parameter_data_thunk;
        callbacks.getParameterSizeCallback = &PluginBase::get_parameter_size_thunk;
        callbacks.setParameterCallback = &PluginBase::set_parameter_thunk;

        return callbacks;
    }
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
}                                                                                         \
                                                                                          \
SENSEKIT_END_DECLS                                                                        \
                                                                                          \
struct missing_semicolon ## __LINE__ {}

#endif /* PLUGINBASE_H */