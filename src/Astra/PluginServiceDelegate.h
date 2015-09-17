/* THIS FILE AUTO-GENERATED FROM PluginServiceDelegate.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEDELEGATE_H
#define PLUGINSERVICEDELEGATE_H

#include <Astra/astra_types.h>
#include <stdarg.h>
#include "AstraContext.h"

namespace astra {

    class PluginServiceDelegate
    {
    public:

        static astra_status_t register_stream_registered_callback(void* pluginService,
                                                                  stream_registered_callback_t callback,
                                                                  void* clientTag,
                                                                  astra_callback_id_t* callbackId)
        {
            return static_cast<PluginService*>(pluginService)->register_stream_registered_callback(callback, clientTag, *callbackId);
        }

        static astra_status_t register_stream_unregistering_callback(void* pluginService,
                                                                     stream_unregistering_callback_t callback,
                                                                     void* clientTag,
                                                                     astra_callback_id_t* callbackId)
        {
            return static_cast<PluginService*>(pluginService)->register_stream_unregistering_callback(callback, clientTag, *callbackId);
        }

        static astra_status_t register_host_event_callback(void* pluginService,
                                                           host_event_callback_t callback,
                                                           void* clientTag,
                                                           astra_callback_id_t* callbackId)
        {
            return static_cast<PluginService*>(pluginService)->register_host_event_callback(callback, clientTag, *callbackId);
        }

        static astra_status_t unregister_host_event_callback(void* pluginService,
                                                             astra_callback_id_t callback)
        {
            return static_cast<PluginService*>(pluginService)->unregister_host_event_callback(callback);
        }

        static astra_status_t unregister_stream_registered_callback(void* pluginService,
                                                                    astra_callback_id_t callback)
        {
            return static_cast<PluginService*>(pluginService)->unregister_stream_registered_callback(callback);
        }

        static astra_status_t unregister_stream_unregistering_callback(void* pluginService,
                                                                       astra_callback_id_t callback)
        {
            return static_cast<PluginService*>(pluginService)->unregister_stream_unregistering_callback(callback);
        }

        static astra_status_t create_stream_set(void* pluginService,
                                                const char* setUri,
                                                astra_streamset_t& setHandle)
        {
            return static_cast<PluginService*>(pluginService)->create_stream_set(setUri, setHandle);
        }

        static astra_status_t destroy_stream_set(void* pluginService,
                                                 astra_streamset_t& setHandle)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream_set(setHandle);
        }

        static astra_status_t get_streamset_uri(void* pluginService,
                                                astra_streamset_t setHandle,
                                                const char** uri)
        {
            return static_cast<PluginService*>(pluginService)->get_streamset_uri(setHandle, *uri);
        }

        static astra_status_t create_stream(void* pluginService,
                                            astra_streamset_t setHandle,
                                            astra_stream_desc_t desc,
                                            stream_callbacks_t pluginCallbacks,
                                            astra_stream_t* handle)
        {
            return static_cast<PluginService*>(pluginService)->create_stream(setHandle, desc, pluginCallbacks, *handle);
        }

        static astra_status_t destroy_stream(void* pluginService,
                                             astra_stream_t& handle)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream(handle);
        }

        static astra_status_t create_stream_bin(void* pluginService,
                                                astra_stream_t streamHandle,
                                                size_t lengthInBytes,
                                                astra_bin_t* binHandle,
                                                astra_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->create_stream_bin(streamHandle, lengthInBytes, *binHandle, *binBuffer);
        }

        static astra_status_t destroy_stream_bin(void* pluginService,
                                                 astra_stream_t streamHandle,
                                                 astra_bin_t* binHandle,
                                                 astra_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream_bin(streamHandle, *binHandle, *binBuffer);
        }

        static astra_status_t bin_has_connections(void* pluginService,
                                                  astra_bin_t binHandle,
                                                  bool* hasConnections)
        {
            return static_cast<PluginService*>(pluginService)->bin_has_connections(binHandle, *hasConnections);
        }

        static astra_status_t cycle_bin_buffers(void* pluginService,
                                                astra_bin_t binHandle,
                                                astra_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->cycle_bin_buffers(binHandle, *binBuffer);
        }

        static astra_status_t link_connection_to_bin(void* pluginService,
                                                     astra_streamconnection_t connection,
                                                     astra_bin_t binHandle)
        {
            return static_cast<PluginService*>(pluginService)->link_connection_to_bin(connection, binHandle);
        }

        static astra_status_t get_parameter_bin(void* pluginService,
                                                size_t byteSize,
                                                astra_parameter_bin_t* binHandle,
                                                astra_parameter_data_t* parameterData)
        {
            return static_cast<PluginService*>(pluginService)->get_parameter_bin(byteSize, *binHandle, *parameterData);
        }

        static astra_status_t log(void* pluginService,
                                  const char* channel,
                                  astra_log_severity_t logLevel,
                                  const char* fileName,
                                  int lineNo,
                                  const char* func,
                                  const char* format,
                                  va_list args)
        {
            return static_cast<PluginService*>(pluginService)->log(channel, logLevel, fileName, lineNo, func, format, args);
        }
    };
}

#endif /* PLUGINSERVICEDELEGATE_H */
