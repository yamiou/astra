#ifndef PLUGINSERVICEPROXYBASE_H
#define PLUGINSERVICEPROXYBASE_H

#include <SenseKit.h>
#include <Plugins/sensekit_plugin_types.h>

typedef void(*StreamAddedCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
typedef void(*StreamRemovingCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
typedef size_t CallbackId;

struct PluginServiceProxyBase
{
    void* pluginService;
    sensekit_status_t (*register_stream_added_callback)(void* service, StreamAddedCallback callback, CallbackId* callbackId);
    sensekit_status_t (*register_stream_removed_callback)(void* service, StreamRemovingCallback callback, CallbackId* callbackId);
    sensekit_status_t (*unregister_stream_added_callback)(void* service, CallbackId callbackId);
    sensekit_status_t (*unregister_stream_removed_callback)(void* service, CallbackId callbackId);
    sensekit_status_t (*create_stream_set)(void* service, sensekit_streamset_t** streamset);
    sensekit_status_t (*destroy_stream_set)(void* service, sensekit_streamset_t** streamset);
    sensekit_status_t (*create_stream)(void* service, StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, stream_callbacks_t pluginCallbacks, /*out*/StreamHandle** handle);
    sensekit_status_t (*destroy_stream)(void* service, /*inout*/StreamHandle** handle);
    sensekit_status_t (*create_stream_bin)(void* service, StreamHandle* handle, size_t lengthInBytes,
                                           /*out*/ StreamBinId* id, /*out*/ sensekit_frame_t** binBuffer);
    sensekit_status_t (*destroy_stream_bin)(void* service, StreamHandle* handle, StreamBinId* id, sensekit_frame_t** old_buf);
    sensekit_status_t (*cycle_bin_buffers)(void* service, StreamHandle* handle, StreamBinId id, sensekit_frame_t** binBuffer);
};

#endif /* PLUGINSERVICEPROXYBASE_H */
