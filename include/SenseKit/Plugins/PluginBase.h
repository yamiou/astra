#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include <SenseKit/sensekit_types.h>
#include <SenseKit/host_events.h>
#include "PluginServiceProxy.h"
#include <SenseKit/Plugins/PluginLogger.h>
#include <cassert>

namespace sensekit {

    class PluginBase
    {
    public:
        PluginBase(PluginServiceProxy* pluginService, const char* pluginName)
            :  m_pluginService(pluginService),
               m_logger(*pluginService, pluginName)
        {
            assert(pluginService != nullptr);
        }

        virtual ~PluginBase() = default;

        void initialize() { on_initialize(); };

        virtual void temp_update() { };

    protected:
        inline PluginServiceProxy& get_pluginService() const  { return *m_pluginService; }
        inline sensekit::plugins::PluginLogger& get_logger() { return m_logger; }

        virtual void on_initialize() { };

        void register_for_stream_events()
        {
            get_pluginService().
                register_stream_registered_callback(&PluginBase::stream_added_handler_thunk,
                                                    this,
                                                    &m_streamAddedCallbackId);

            get_pluginService().
                register_stream_unregistering_callback(&PluginBase::stream_removing_handler_thunk,
                                                       this,
                                                       &m_streamRemovingCallbackId);
        }

        void register_for_host_events()
        {

            get_pluginService().
                register_host_event_callback(&PluginBase::host_event_handler_thunk,
                                             this,
                                             &m_hostEventCallbackId);
        }

        void unregister_for_stream_events()
        {
            get_pluginService().unregister_stream_registered_callback(m_streamAddedCallbackId);
            get_pluginService().unregister_stream_unregistering_callback(m_streamRemovingCallbackId);
        }

        void unregister_for_host_events()
        {
            get_pluginService().unregister_host_event_callback(m_hostEventCallbackId);
        }

    private:

        PluginServiceProxy* m_pluginService{nullptr};
        sensekit::plugins::PluginLogger m_logger;

        static void stream_added_handler_thunk(void* clientTag,
                                               sensekit_streamset_t setHandle,
                                               sensekit_stream_t streamHandle,
                                               sensekit_stream_desc_t desc)
        {
            PluginBase* plugin = static_cast<PluginBase*>(clientTag);
            plugin->stream_added_handler(setHandle, streamHandle, desc);
        }

        static void stream_removing_handler_thunk(void* clientTag,
                                                  sensekit_streamset_t setHandle,
                                                  sensekit_stream_t streamHandle,
                                                  sensekit_stream_desc_t desc)

        {
            PluginBase* plugin = static_cast<PluginBase*>(clientTag);
            plugin->stream_removing_handler(setHandle, streamHandle, desc);
        }

        static void host_event_handler_thunk(void* clientTag,
                                             sensekit_event_id id,
                                             const void* data,
                                             size_t dataSize)
        {
            PluginBase* plugin = static_cast<PluginBase*>(clientTag);
            plugin->host_event_handler(id, data, dataSize);
        }

        void stream_added_handler(sensekit_streamset_t setHandle,
                                  sensekit_stream_t streamHandle,
                                  sensekit_stream_desc_t desc)
        {
            on_stream_added(setHandle, streamHandle, desc);
        }

        void stream_removing_handler(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t desc)
        {
            on_stream_removed(setHandle, streamHandle, desc);
        }

        void host_event_handler(sensekit_event_id id,
                                const void* data,
                                size_t dataSize)
        {
            on_host_event(id, data, dataSize);
        }

        virtual void on_stream_added(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t desc)
        {}

        virtual void on_stream_removed(sensekit_streamset_t setHandle,
                                       sensekit_stream_t streamHandle,
                                       sensekit_stream_desc_t desc)
        {}

        virtual void on_host_event(sensekit_event_id id, const void* data, size_t dataSize) {}

        sensekit_callback_id_t m_streamAddedCallbackId;
        sensekit_callback_id_t m_streamRemovingCallbackId;
        sensekit_callback_id_t m_hostEventCallbackId;
    };
}

#define EXPORT_PLUGIN(className)                                        \
                                                                        \
    static className* g_plugin;                                         \
                                                                        \
    SENSEKIT_BEGIN_DECLS                                                \
                                                                        \
    SENSEKIT_EXPORT void sensekit_plugin_initialize(PluginServiceProxyBase* pluginProxy) \
    {                                                                   \
        g_plugin = new className(                                       \
            static_cast<sensekit::PluginServiceProxy*>(pluginProxy));   \
        g_plugin->initialize();                                         \
    }                                                                   \
                                                                        \
    SENSEKIT_EXPORT void sensekit_plugin_update()                       \
    {                                                                   \
        g_plugin->temp_update();                                        \
    }                                                                   \
                                                                        \
    SENSEKIT_EXPORT void sensekit_plugin_terminate()                    \
    {                                                                   \
        delete g_plugin;                                                \
        g_plugin = nullptr;                                             \
    }                                                                   \
                                                                        \
    SENSEKIT_END_DECLS

#endif /* PLUGINBASE_H */