#ifndef PLUGINSERVICEDELEGATE_H
#define PLUGINSERVICEDELEGATE_H

#include <sensekit_core.h>
#include "PluginService.h"
#include "SenseKitContext.h"

namespace sensekit {

    class PluginServiceDelegate {

    public:
        static sensekit_status_t register_stream_added_callback(void* service, StreamAddedCallback callback, CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_added_callback(callback, *callbackId);
            }
        static sensekit_status_t register_stream_removing_callback(void* service, StreamRemovingCallback callback, CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_removing_callback(callback, *callbackId);
            }
        static sensekit_status_t unregister_stream_added_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_added_callback(callbackId);
            }
        static sensekit_status_t unregister_stream_removing_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_removing_callback(callbackId);
            }
        static sensekit_status_t create_stream_set(void* service, /*out*/StreamSetHandle** setHandle)
            {
                return static_cast<PluginService*>(service)->create_stream_set(*setHandle);
            }
        static sensekit_status_t destroy_stream_set(void* service, /*inout*/StreamSetHandle** setHandle)
            {
                return static_cast<PluginService*>(service)->destroy_stream_set(*setHandle);
            }
        static sensekit_status_t create_stream(void* service, StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, stream_callbacks_t pluginCallbacks, /*out*/StreamHandle** handle)
            {
                return static_cast<PluginService*>(service)->create_stream(setHandle, type, subtype, pluginCallbacks, *handle);
            }
        static sensekit_status_t destroy_stream(void* service, StreamHandle** handle)
            {
                return static_cast<PluginService*>(service)->destroy_stream(*handle);
            }
        static sensekit_status_t create_stream_bin(void* service, StreamHandle* handle, size_t lengthInBytes,
                                                   /*out*/ StreamBinId* id, /*out*/ sensekit_frame_t** binBuffer)
            {
                return static_cast<PluginService*>(service)->create_stream_bin(handle, lengthInBytes, *id, *binBuffer);
            }
        static sensekit_status_t destroy_stream_bin(void* service, StreamHandle* handle, StreamBinId* id, sensekit_frame_t** old_buf)
            {
                return static_cast<PluginService*>(service)->destroy_stream_bin(handle, *id, *old_buf);
            }
        static sensekit_status_t cycle_bin_buffers(void* service, StreamHandle* handle, StreamBinId id, sensekit_frame_t** binBuffer)
            {
                return static_cast<PluginService*>(service)->cycle_bin_buffers(handle, id, *binBuffer);
            }
    };
}

#endif /* PLUGINSERVICEDELEGATE_H */
