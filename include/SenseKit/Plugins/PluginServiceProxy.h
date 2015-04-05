#ifndef PLUGINSERVICEPROXY_H
#define PLUGINSERVICEPROXY_H

#include "PluginServiceProxyBase.h"

namespace sensekit {

    class PluginServiceProxy : public PluginServiceProxyBase
    {
    public:
        sensekit_status_t register_stream_added_callback(StreamAddedCallback callback, CallbackId* callbackId)
            {
                return PluginServiceProxyBase::register_stream_added_callback(pluginService, callback, callbackId);
            }

        sensekit_status_t register_stream_removing_callback(StreamRemovingCallback callback, CallbackId* callbackId)
            {
                return PluginServiceProxyBase::register_stream_removing_callback(pluginService, callback, callbackId);
            }

        sensekit_status_t unregister_stream_added_callback(CallbackId callbackId)
            {
                return PluginServiceProxyBase::unregister_stream_added_callback(pluginService, callbackId);
            }

        sensekit_status_t unregister_stream_removing_callback(CallbackId callbackId)
            {
                return PluginServiceProxyBase::unregister_stream_removing_callback(pluginService, callbackId);
            }

        sensekit_status_t create_stream_set(StreamSetHandle*& setHandle)
            {
                return PluginServiceProxyBase::create_stream_set(pluginService, &setHandle);
            }

        sensekit_status_t destroy_stream_set(StreamSetHandle*& setHandle)
            {
                return PluginServiceProxyBase::destroy_stream_set(pluginService, &setHandle);
            }

        sensekit_status_t create_stream(StreamSetHandle* setHandle,
                                        sensekit_stream_desc_t desc,
                                        stream_callbacks_t pluginCallbacks,
                                        sensekit_stream_handle_t* handle)
            {
                return PluginServiceProxyBase::create_stream(pluginService, setHandle, desc, pluginCallbacks, handle);
            }

        sensekit_status_t destroy_stream(sensekit_stream_handle_t& handle)
            {
                return PluginServiceProxyBase::destroy_stream(pluginService, &handle);
            }

        sensekit_status_t create_stream_bin(sensekit_stream_handle_t streamHandle,
                                            size_t lengthInBytes,
                                            sensekit_bin_handle_t* binHandle,
                                            sensekit_frame_t** binBuffer)
            {
                return PluginServiceProxyBase::create_stream_bin(pluginService,
                                                                 streamHandle,
                                                                 lengthInBytes,
                                                                 binHandle,
                                                                 binBuffer);
            }

        sensekit_status_t destroy_stream_bin(sensekit_stream_handle_t handle,
                                             sensekit_bin_handle_t* binHandle,
                                             sensekit_frame_t** binBuffer)
            {
                return PluginServiceProxyBase::destroy_stream_bin(pluginService, handle, binHandle, binBuffer);
            }

        sensekit_status_t cycle_bin_buffers(sensekit_bin_handle_t binHandle,
                                            sensekit_frame_t** binBuffer)
            {
                return PluginServiceProxyBase::cycle_bin_buffers(pluginService, binHandle, binBuffer);
            }

        sensekit_status_t link_connection_to_bin(sensekit_streamconnection_t* connection,
                                                 sensekit_bin_handle_t binHandle)
            {
                return PluginServiceProxyBase::link_connection_to_bin(pluginService, connection, binHandle);
            }
    };
}
#endif /* PLUGINSERVICEPROXY_H */
