#ifndef PLUGINTYPES_H
#define PLUGINTYPES_H

#include "sensekit_core.h"
#include "sensekit_types.h"
#include "../../include/sensekit_plugin_types.h"

typedef void(*StreamAddedCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
typedef void(*StreamRemovingCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
typedef size_t CallbackId;

struct PluginServiceProxyBase
{
    void* pluginService;
    void* streamService;
    sensekit_status_t (*register_stream_added_callback)(void* service, StreamAddedCallback callback, CallbackId* callbackId);
    sensekit_status_t (*register_stream_removed_callback)(void* service, StreamRemovingCallback callback, CallbackId* callbackId);
    sensekit_status_t (*unregister_stream_added_callback)(void* service, CallbackId callbackId);
    sensekit_status_t (*unregister_stream_removed_callback)(void* service, CallbackId callbackId);
    sensekit_status_t (*create_stream)(void* service, StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks, /*out*/StreamHandle** handle);
    sensekit_status_t (*destroy_stream)(void* service, /*inout*/StreamHandle** handle);
    sensekit_status_t (*create_stream_bin)(void* service, StreamHandle* handle, size_t lengthInBytes,
                                           /*out*/ StreamBinId* id, /*out*/ sensekit_frame_t** binBuffer);
    sensekit_status_t (*destroy_stream_bin)(void* service, StreamHandle* handle, StreamBinId* id, sensekit_frame_t** old_buf);
    sensekit_status_t (*cycle_bin_buffers)(void* service, StreamHandle* handle, StreamBinId id, sensekit_frame_t** binBuffer);

    sensekit_status_t (*open_streamset)(void* service, const char* uri, sensekit_streamset_t** streamset);
    sensekit_status_t (*close_streamset)(void* service, sensekit_streamset_t** streamset);
    sensekit_status_t (*open_stream)(void* service, sensekit_streamset_t* streamset, sensekit_stream_type_t type, sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection);
    sensekit_status_t (*close_stream)(void* service, sensekit_streamconnection_t** stream_connection);
    sensekit_status_t (*open_frame)(void* service, sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frameRef);
    sensekit_status_t (*close_frame)(void* service, sensekit_frame_ref_t** frameRef);
};

#endif /* PLUGINTYPES_H */
