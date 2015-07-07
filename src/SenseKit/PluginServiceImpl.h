/* THIS FILE AUTO-GENERATED FROM PluginServiceImpl.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEIMPL_H
#define PLUGINSERVICEIMPL_H

#include <SenseKit/sensekit_types.h>
#include "Stream.h"
#include "StreamBin.h"
#include "Core/Signal.h"
#include "Logger.h"

using CallbackId = size_t;

struct PluginServiceImplProxyBase;

namespace sensekit
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

        void notify_host_event(sensekit_event_id id, const void* data, size_t dataSize);

        sensekit_status_t register_stream_registered_callback(stream_registered_callback_t callback,
                                                              void* clientTag,
                                                              sensekit_callback_id_t& callbackId);
        sensekit_status_t register_stream_unregistering_callback(stream_unregistering_callback_t callback,
                                                                 void* clientTag,
                                                                 sensekit_callback_id_t& callbackId);
        sensekit_status_t register_host_event_callback(host_event_callback_t callback,
                                                       void* clientTag,
                                                       sensekit_callback_id_t& callbackId);
        sensekit_status_t unregister_host_event_callback(sensekit_callback_id_t callback);
        sensekit_status_t unregister_stream_registered_callback(sensekit_callback_id_t callback);
        sensekit_status_t unregister_stream_unregistering_callback(sensekit_callback_id_t callback);
        sensekit_status_t create_stream_set(const char* setUri,
                                            sensekit_streamset_t& setHandle);
        sensekit_status_t destroy_stream_set(sensekit_streamset_t& setHandle);
        sensekit_status_t get_streamset_uri(sensekit_streamset_t setHandle,
                                            const char*& uri);
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
        sensekit_status_t get_parameter_bin(size_t byteSize,
                                            sensekit_parameter_bin_t& binHandle,
                                            sensekit_parameter_data_t& parameterData);
        sensekit_status_t log(const char* channel,
                              sensekit_log_severity_t logLevel,
                              const char* fileName,
                              int lineNo,
                              const char* func,
                              const char* format,
                              va_list args);

    private:
        StreamSetCatalog& m_setCatalog;
        Signal<sensekit_event_id, const void*, size_t> m_hostEventSignal;
    };
}

#endif /* PLUGINSERVICEIMPL_H */
