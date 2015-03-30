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

        sensekit_status_t create_stream_set(/*out*/StreamSetHandle*& setHandle)
            {
                return PluginServiceProxyBase::create_stream_set(pluginService, &setHandle);
            }

        sensekit_status_t destroy_stream_set(/*inout*/StreamSetHandle*& setHandle)
            {
                return PluginServiceProxyBase::destroy_stream_set(pluginService, &setHandle);
            }

        sensekit_status_t create_stream(StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, stream_callbacks_t pluginCallbacks, /*out*/StreamHandle*& handle)
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
    };
}
#endif /* PLUGINSERVICEPROXY_H */
