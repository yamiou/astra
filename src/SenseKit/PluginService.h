#ifndef PLUGINSERVICE_H
#define PLUGINSERVICE_H

#include <SenseKit.h>
#include "Stream.h"
#include "StreamBin.h"
#include "Core/Signal.h"

//typedef void(*StreamAddedCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
//typedef void(*StreamRemovingCallback)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
//typedef size_t CallbackId;

using StreamAddedCallback = void(*)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
using StreamRemovingCallback = void(*)(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType typeId, StreamSubtype subtype);
using CallbackId = size_t;

struct PluginServiceProxyBase;

namespace sensekit
{
    class SenseKitContext;
    class StreamSet;

    class PluginService
    {
    public:
        PluginService(SenseKitContext& context)
            : m_context(context)
            {}

        PluginServiceProxyBase* create_proxy();

        sensekit_status_t create_stream_set(sensekit_streamset_t*& streamset);
        sensekit_status_t destroy_stream_set(sensekit_streamset_t*& streamset);

        // metadata = int num_stream_types, StreamTypeId[] ids
        //for generators (no requirements, i.e. depth sensor and color sensor)
        //plugin would directly create and register the streams, without using stream_factory

        sensekit_status_t register_stream_added_callback(StreamAddedCallback callback, CallbackId& callbackId);
        sensekit_status_t register_stream_removing_callback(StreamRemovingCallback callback, CallbackId& callbackId);
        sensekit_status_t unregister_stream_added_callback(CallbackId callbackId);
        sensekit_status_t unregister_stream_removing_callback(CallbackId callbackId);

        // Plugin notifying framework of a newly available stream
        sensekit_status_t create_stream(StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks, /*out*/StreamHandle*& handle);

        // Plugin notifying framework of a stream no longer available
        sensekit_status_t destroy_stream(StreamHandle*& handle);

        // TODO: create BinHandle, retire BinId, pass BinHandles to plugins
        sensekit_status_t create_stream_bin(StreamHandle* handle, size_t lengthInBytes,
                                            /*out*/ StreamBinId& id, /*out*/ sensekit_frame_t*& binBuffer);

        sensekit_status_t destroy_stream_bin(StreamHandle* handle, StreamBinId& id, sensekit_frame_t*& old_buf);

        sensekit_status_t cycle_bin_buffers(StreamHandle* handle, StreamBinId id, sensekit_frame_t*& binBuffer);

        //orbbec_error orbbec_stream_subscribe_client_added_event(StreamHandle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_subscribe_client_removed_event(StreamHandle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_assign_connection_to_bin(StreamHandle handle, client_id id, bin_id id);
        //orbbec_error orbbec_stream_register_get_parameter_callback(component_handle handle, client_id client, ...);
        //orbbec_error orbbec_stream_register_set_parameter_callback(component_handle handle, client_id client, ...);
        //callback gets passed a context_id

        //orbbec_error orbbec_unregister_stream_factory(...);
        //nominally on plugin shutdown

    private:
        SenseKitContext& m_context;
        Signal<StreamSetHandle*, StreamHandle*, StreamType, StreamSubtype> m_streamAddedSignal;
        Signal<StreamSetHandle*, StreamHandle*, StreamType, StreamSubtype> m_streamRemovingSignal;

    };
}

#endif /* PLUGINSERVICE_H */