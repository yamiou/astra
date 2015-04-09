/* THIS FILE AUTO-GENERATED FROM PluginService.h.lpp. DO NOT EDIT. */
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

        sensekit_status_t register_stream_added_callback(stream_added_callback_t callback,
                                                         void* clientTag,
                                                         sensekit_callback_id_t& callbackId);
        sensekit_status_t register_stream_removing_callback(stream_removing_callback_t callback,
                                                            void* clientTag,
                                                            sensekit_callback_id_t& callbackId);
        sensekit_status_t unregister_stream_added_callback(sensekit_callback_id_t callback);
        sensekit_status_t unregister_stream_removing_callback(sensekit_callback_id_t callback);
        sensekit_status_t create_stream_set(sensekit_streamset_t& setHandle);
        sensekit_status_t destroy_stream_set(sensekit_streamset_t& setHandle);
        sensekit_status_t create_stream(sensekit_streamset_t setHandle,
                                        sensekit_stream_desc_t desc,
                                        stream_callbacks_t pluginCallbacks,
                                        sensekit_stream_t& handle);
        sensekit_status_t destroy_stream(sensekit_stream_t& handle);
        sensekit_status_t create_stream_bin(sensekit_stream_t streamHandle,
                                            size_t lengthInBytes,
                                            sensekit_bin_t& binHandle,
                                            sensekit_frame_t*& binBuffer);
        sensekit_status_t destroy_stream_bin(sensekit_stream_t streamHandle,
                                             sensekit_bin_t& binHandle,
                                             sensekit_frame_t*& binBuffer);
        sensekit_status_t bin_has_connections(sensekit_bin_t binHandle,
                                              bool& hasConnections);
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
