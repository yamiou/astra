#ifndef PLUGINSERVICEDELEGATE_H
#define PLUGINSERVICEDELEGATE_H

#include "sensekit_types.h"
#include "PluginService.h"
#include "SenseKitContext.h"

namespace sensekit {

class PluginServiceDelegate {
public:
        static sensekit_status_t register_stream_added_callback(void* service, StreamAddedCallback callback, CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_added_callback(callback, *callbackId);
            }
        static sensekit_status_t register_stream_removed_callback(void* service, StreamRemovingCallback callback, CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_removing_callback(callback, *callbackId);
            }
        static sensekit_status_t unregister_stream_added_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_added_callback(callbackId);
            }
        static sensekit_status_t unregister_stream_removed_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_removing_callback(callbackId);
            }
        static sensekit_status_t create_stream(void* service, StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks, /*out*/StreamHandle** handle)
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

        static sensekit_status_t open_streamset(void* service, const char* uri, sensekit_streamset_t** streamset)
            {
                return static_cast<SenseKitContext*>(service)->open_streamset(uri, *streamset);
            }
        static sensekit_status_t close_streamset(void* service, sensekit_streamset_t** streamset)
            {
                return static_cast<SenseKitContext*>(service)->close_streamset(*streamset);
            }
        static sensekit_status_t open_stream(void* service, sensekit_streamset_t* streamset, sensekit_stream_type_t type, sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
            {
                return static_cast<SenseKitContext*>(service)->open_stream(streamset, type, subtype, *stream_connection);
            }
        static sensekit_status_t close_stream(void* service, sensekit_streamconnection_t** stream_connection)
            {
                return static_cast<SenseKitContext*>(service)->close_stream(*stream_connection);
            }
        static sensekit_status_t open_frame(void* service, sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->open_frame(stream_connection, timeout, *frameRef);
            }
        static sensekit_status_t close_frame(void* service, sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->close_frame(*frameRef);
            }
    };
}

#endif /* PLUGINSERVICEDELEGATE_H */
