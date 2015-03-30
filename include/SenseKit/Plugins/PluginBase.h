#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginServiceProxy.h"
#include "StreamServiceProxy.h"
#include <Plugins/sensekit_plugin_types.h>

namespace sensekit {

    class PluginBase
    {
    public:
        PluginBase(StreamServiceProxy* streamService, PluginServiceProxy* pluginService)
            :
            m_streamService(streamService),
            m_pluginService(pluginService)
            {};

        virtual ~PluginBase() = default;

        //stream core calls these on plugins
        //TODO transition this init call to the PluginBase ctor
        void initialize()
            {
                if (m_initialized)
                    return;

                on_initialize();

                m_initialized = true;
            }

        void cleanup()
            {
                if (!m_initialized)
                    return;

                on_cleanup();

                m_initialized = false;
            }

        virtual void temp_update() = 0;
        bool is_initialized() const { return m_initialized; }

    protected:
        inline PluginServiceProxy& get_pluginService() const  { return *m_pluginService; }
        inline StreamServiceProxy& get_streamService() const { return *m_streamService; }

        virtual void on_initialize() {}
        virtual void on_cleanup() {}

    private:
        bool m_initialized{false};
        PluginServiceProxy* m_pluginService;
        StreamServiceProxy* m_streamService;

        static void set_parameter_thunk(void* instance,
                                        sensekit_streamconnection_t* connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data)
            {
                static_cast<PluginBase*>(instance)->set_parameter(connection, id, byteLength, data);
            }

        static void get_parameter_size_thunk(void* instance,
                                             sensekit_streamconnection_t* connection,
                                             sensekit_parameter_id id,
                                             size_t* byteLength)
            {
                static_cast<PluginBase*>(instance)->get_parameter_size(connection, id, *byteLength);
            }

        static void get_parameter_data_thunk(void* instance,
                                             sensekit_streamconnection_t* connection,
                                             sensekit_parameter_id id,
                                             size_t byteLength,
                                             sensekit_parameter_data_t* data)
            {
                static_cast<PluginBase*>(instance)->get_parameter_data(connection, id, byteLength, data);
            }

        static void connection_added_thunk(void* instance,
                                           sensekit_streamconnection_t* connection)
            {
                static_cast<PluginBase*>(instance)->connection_added(connection);
            }

        static void connection_removed_thunk(void* instance,
                                             sensekit_streamconnection_t* connection)
            {
                static_cast<PluginBase*>(instance)->connection_removed(connection);
            }

        virtual void set_parameter(sensekit_streamconnection_t* connection,
                                   sensekit_parameter_id id,
                                   size_t byteLength,
                                   sensekit_parameter_data_t* data) {}

        virtual void get_parameter_size(sensekit_streamconnection_t* connection,
                                        sensekit_parameter_id id,
                                        size_t& byteLength) {}

        virtual void get_parameter_data(sensekit_streamconnection_t* connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data) {}

        virtual void connection_added(sensekit_streamconnection_t* connection) {}
        virtual void connection_removed(sensekit_streamconnection_t* connection) {}

        friend stream_callbacks_t create_plugin_callbacks(PluginBase* context);

    };

    stream_callbacks_t create_plugin_callbacks(PluginBase* context)
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

#endif /* PLUGINBASE_H */