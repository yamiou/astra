/* THIS FILE AUTO-GENERATED FROM PluginServiceImpl.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEIMPL_H
#define PLUGINSERVICEIMPL_H

#include <Astra/astra_types.h>
#include "Stream.h"
#include "StreamBin.h"
#include "Core/Signal.h"
#include "Logger.h"

using CallbackId = size_t;

struct PluginServiceImplProxyBase;

namespace astra
{
    class StreamSet;
    class StreamSetCatalog;

    class PluginServiceImpl
    {
    public:
        PluginServiceImpl(StreamSetCatalog& catalog)
            : m_setCatalog(catalog)
            {}

        PluginServiceImpl(const PluginServiceImpl& service) = delete;
        PluginServiceImpl& operator=(const PluginServiceImpl& rhs) = delete;

        void notify_host_event(astra_event_id id, const void* data, size_t dataSize);

        astra_status_t register_stream_registered_callback(stream_registered_callback_t callback,
                                                           void* clientTag,
                                                           astra_callback_id_t& callbackId);
        astra_status_t register_stream_unregistering_callback(stream_unregistering_callback_t callback,
                                                              void* clientTag,
                                                              astra_callback_id_t& callbackId);
        astra_status_t register_host_event_callback(host_event_callback_t callback,
                                                    void* clientTag,
                                                    astra_callback_id_t& callbackId);
        astra_status_t unregister_host_event_callback(astra_callback_id_t callback);
        astra_status_t unregister_stream_registered_callback(astra_callback_id_t callback);
        astra_status_t unregister_stream_unregistering_callback(astra_callback_id_t callback);
        astra_status_t create_stream_set(const char* setUri,
                                         astra_streamset_t& setHandle);
        astra_status_t destroy_stream_set(astra_streamset_t& setHandle);
        astra_status_t get_streamset_uri(astra_streamset_t setHandle,
                                         const char*& uri);
        astra_status_t create_stream(astra_streamset_t setHandle,
                                     astra_stream_desc_t desc,
                                     stream_callbacks_t pluginCallbacks,
                                     astra_stream_t& handle);
        astra_status_t destroy_stream(astra_stream_t& handle);
        astra_status_t create_stream_bin(astra_stream_t streamHandle,
                                         size_t lengthInBytes,
                                         astra_bin_t& binHandle,
                                         astra_frame_t*& binBuffer);
        astra_status_t destroy_stream_bin(astra_stream_t streamHandle,
                                          astra_bin_t& binHandle,
                                          astra_frame_t*& binBuffer);
        astra_status_t bin_has_connections(astra_bin_t binHandle,
                                           bool& hasConnections);
        astra_status_t cycle_bin_buffers(astra_bin_t binHandle,
                                         astra_frame_t*& binBuffer);
        astra_status_t link_connection_to_bin(astra_streamconnection_t connection,
                                              astra_bin_t binHandle);
        astra_status_t get_parameter_bin(size_t byteSize,
                                         astra_parameter_bin_t& binHandle,
                                         astra_parameter_data_t& parameterData);
        astra_status_t log(const char* channel,
                           astra_log_severity_t logLevel,
                           const char* fileName,
                           int lineNo,
                           const char* func,
                           const char* format,
                           va_list args);

    private:
        StreamSetCatalog& m_setCatalog;
        Signal<astra_event_id, const void*, size_t> m_hostEventSignal;
    };
}

#endif /* PLUGINSERVICEIMPL_H */
