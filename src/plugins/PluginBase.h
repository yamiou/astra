#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginTypes.h"

namespace sensekit
{

class PluginServiceProxy : public PluginServiceProxyBase
{
public:

    sensekit_status_t register_stream_added_callback(StreamAddedCallback callback, CallbackId* callbackId)
        {
            return PluginServiceProxyBase::register_stream_added_callback(pluginService, callback, callbackId);
        }

    sensekit_status_t register_stream_removed_callback(StreamRemovingCallback callback, CallbackId* callbackId)
        {
            return PluginServiceProxyBase::register_stream_removed_callback(pluginService, callback, callbackId);
        }

    sensekit_status_t unregister_stream_added_callback(CallbackId callbackId)
        {
            return PluginServiceProxyBase::unregister_stream_added_callback(pluginService, callbackId);
        }

    sensekit_status_t unregister_stream_removed_callback(CallbackId callbackId)
        {
            return PluginServiceProxyBase::unregister_stream_removed_callback(pluginService, callbackId);
        }

    sensekit_status_t create_stream(StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks, /*out*/StreamHandle*& handle)
        {
            return PluginServiceProxyBase::create_stream(pluginService, setHandle, type, subtype, pluginCallbacks, &handle);
        }

    sensekit_status_t destroy_stream(StreamHandle*& handle)
        {
            return PluginServiceProxyBase::destroy_stream(pluginService, &handle);
        }

    sensekit_status_t create_stream_bin(StreamHandle* handle, size_t lengthInBytes,
                                        /*out*/ StreamBinId* id, /*out*/ sensekit_frame_t** binBuffer)
        {
            return PluginServiceProxyBase::create_stream_bin(pluginService, handle, lengthInBytes, id, binBuffer);
        }

    sensekit_status_t destroy_stream_bin(StreamHandle* handle, StreamBinId* id, sensekit_frame_t** buffer)
        {
            return PluginServiceProxyBase::destroy_stream_bin(pluginService, handle, id, buffer);
        }

    sensekit_status_t cycle_bin_buffers(StreamHandle* handle, StreamBinId id, sensekit_frame_t** binBuffer)
        {
            return PluginServiceProxyBase::cycle_bin_buffers(pluginService, handle, id, binBuffer);
        }

    sensekit_status_t open_streamset(const char* uri, sensekit_streamset_t** streamset)
        {
            return PluginServiceProxyBase::open_streamset(streamService, uri, streamset);
        }

    sensekit_status_t close_streamset(sensekit_streamset_t** streamset)
        {
            return PluginServiceProxyBase::close_streamset(streamService, streamset);
        }

    sensekit_status_t open_stream(sensekit_streamset_t* streamset, sensekit_stream_type_t type, sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
        {
            return PluginServiceProxyBase::open_stream(streamService, streamset, type, subtype, stream_connection);
        }

    sensekit_status_t close_stream(sensekit_streamconnection_t** stream_connection)
        {
            return PluginServiceProxyBase::close_stream(streamService, stream_connection);
        }

    sensekit_status_t open_frame(sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frameRef)
        {
            return PluginServiceProxyBase::open_frame(streamService, stream_connection, timeout, frameRef);
        }

    sensekit_status_t close_frame(sensekit_frame_ref_t** frameRef)
        {
            return PluginServiceProxyBase::close_frame(streamService, frameRef);
        }
};

    class PluginBase
    {
    public:
        PluginBase(PluginServiceProxy* pluginService)
            : m_pluginService(pluginService)
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

        virtual void on_initialize() {}
        virtual void on_cleanup() {}

    private:
        bool m_initialized{false};
        PluginServiceProxy* m_pluginService;
    };
}

#endif /* PLUGINBASE_H */