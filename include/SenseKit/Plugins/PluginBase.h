#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginServiceProxy.h"
#include "StreamServiceProxy.h"
#include <Plugins/sensekit_plugin_types.h>

namespace sensekit
{
    StreamPluginCallbacks create_plugin_callbacks(void* context)
    {
        StreamPluginCallbacks callbacks;

        callbacks.context = context;
        callbacks.connectionAddedCallback = nullptr;
        callbacks.connectionRemovedCallback = nullptr;
        callbacks.getParameterDataCallback = nullptr;
        callbacks.getParameterSizeCallback = nullptr;
        callbacks.setParameterCallback = nullptr;

        return callbacks;
    }

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
    };
}

#endif /* PLUGINBASE_H */