#ifndef PLUGINSERVICE_H
#define PLUGINSERVICE_H

#include <sensekit_core.h>
#include "Stream.h"
#include "StreamBin.h"
#include "Core/Signal.h"

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

        sensekit_status_t create_stream_set(sensekit_streamset_t& streamSet);
        sensekit_status_t destroy_stream_set(sensekit_streamset_t& streamSet);

        // metadata = int num_stream_types, StreamTypeId[] ids
        //for generators (no requirements, i.e. depth sensor and color sensor)
        //plugin would directly create and register the streams, without using stream_factory

        sensekit_status_t register_stream_added_callback(stream_added_callback_t callback, CallbackId& callbackId);
        sensekit_status_t register_stream_removing_callback(stream_removing_callback_t callback, CallbackId& callbackId);
        sensekit_status_t unregister_stream_added_callback(CallbackId callbackId);
        sensekit_status_t unregister_stream_removing_callback(CallbackId callbackId);

        // Plugin notifying framework of a newly available stream
        sensekit_status_t create_stream(sensekit_streamset_t setHandle,
                                        sensekit_stream_desc_t desc,
                                        stream_callbacks_t pluginCallbacks,
                                        sensekit_stream_t& streamHandle);

        // Plugin notifying framework of a stream no longer available
        sensekit_status_t destroy_stream(sensekit_stream_t& handle);

        sensekit_status_t create_stream_bin(sensekit_stream_t handle,
                                            size_t lengthInBytes,
                                            sensekit_bin_t& id,
                                            sensekit_frame_t*& binBuffer);

        sensekit_status_t destroy_stream_bin(sensekit_stream_t streamHandle,
                                             sensekit_bin_t& binHandle,
                                             sensekit_frame_t*& binBuffer);

        sensekit_status_t cycle_bin_buffers(sensekit_bin_t binHandle,
                                            sensekit_frame_t*& binBuffer);

        sensekit_status_t link_connection_to_bin(sensekit_streamconnection_t connection,
                                                 sensekit_bin_t binHandle);

    private:
        SenseKitContext& m_context;
        Signal<sensekit_streamset_t, sensekit_stream_t, sensekit_stream_desc_t> m_streamAddedSignal;
        Signal<sensekit_streamset_t, sensekit_stream_t, sensekit_stream_desc_t> m_streamRemovingSignal;

    };
}

#endif /* PLUGINSERVICE_H */