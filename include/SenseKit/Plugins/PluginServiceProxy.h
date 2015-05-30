/* THIS FILE AUTO-GENERATED FROM PluginServiceProxy.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEPROXY_H
#define PLUGINSERVICEPROXY_H

#include "PluginServiceProxyBase.h"
#include <cstdarg>
#include <cstdio>

namespace sensekit {

    class PluginServiceProxy : public PluginServiceProxyBase
    {
    public:

    sensekit_status_t register_stream_added_callback(stream_added_callback_t callback,
                                                     void* clientTag,
                                                     sensekit_callback_id_t* callbackId)
    {
        return PluginServiceProxyBase::register_stream_added_callback(pluginService, callback, clientTag, callbackId);
    }

    sensekit_status_t register_stream_removing_callback(stream_removing_callback_t callback,
                                                        void* clientTag,
                                                        sensekit_callback_id_t* callbackId)
    {
        return PluginServiceProxyBase::register_stream_removing_callback(pluginService, callback, clientTag, callbackId);
    }

    sensekit_status_t register_host_event_callback(host_event_callback_t callback,
                                                   void* clientTag,
                                                   sensekit_callback_id_t* callbackId)
    {
        return PluginServiceProxyBase::register_host_event_callback(pluginService, callback, clientTag, callbackId);
    }

    sensekit_status_t unregister_host_event_callback(sensekit_callback_id_t callback)
    {
        return PluginServiceProxyBase::unregister_host_event_callback(pluginService, callback);
    }

    sensekit_status_t unregister_stream_added_callback(sensekit_callback_id_t callback)
    {
        return PluginServiceProxyBase::unregister_stream_added_callback(pluginService, callback);
    }

    sensekit_status_t unregister_stream_removing_callback(sensekit_callback_id_t callback)
    {
        return PluginServiceProxyBase::unregister_stream_removing_callback(pluginService, callback);
    }

    sensekit_status_t create_stream_set(const char* setUri,
                                        sensekit_streamset_t& setHandle)
    {
        return PluginServiceProxyBase::create_stream_set(pluginService, setUri, setHandle);
    }

    sensekit_status_t destroy_stream_set(sensekit_streamset_t& setHandle)
    {
        return PluginServiceProxyBase::destroy_stream_set(pluginService, setHandle);
    }

    sensekit_status_t connect_to_streamset(sensekit_streamset_t setHandle,
                                           sensekit_streamsetconnection_t& conn)
    {
        return PluginServiceProxyBase::connect_to_streamset(pluginService, setHandle, conn);
    }

    sensekit_status_t get_streamset_from_streamsetconnection(sensekit_streamsetconnection_t connection,
                                                             sensekit_streamset_t& setHandle)
    {
        return PluginServiceProxyBase::get_streamset_from_streamsetconnection(pluginService, connection, setHandle);
    }

    sensekit_status_t create_stream(sensekit_streamset_t setHandle,
                                    sensekit_stream_desc_t desc,
                                    stream_callbacks_t pluginCallbacks,
                                    sensekit_stream_t* handle)
    {
        return PluginServiceProxyBase::create_stream(pluginService, setHandle, desc, pluginCallbacks, handle);
    }

    sensekit_status_t destroy_stream(sensekit_stream_t& handle)
    {
        return PluginServiceProxyBase::destroy_stream(pluginService, handle);
    }

    sensekit_status_t create_stream_bin(sensekit_stream_t streamHandle,
                                        size_t lengthInBytes,
                                        sensekit_bin_t* binHandle,
                                        sensekit_frame_t** binBuffer)
    {
        return PluginServiceProxyBase::create_stream_bin(pluginService, streamHandle, lengthInBytes, binHandle, binBuffer);
    }

    sensekit_status_t destroy_stream_bin(sensekit_stream_t streamHandle,
                                         sensekit_bin_t* binHandle,
                                         sensekit_frame_t** binBuffer)
    {
        return PluginServiceProxyBase::destroy_stream_bin(pluginService, streamHandle, binHandle, binBuffer);
    }

    sensekit_status_t bin_has_connections(sensekit_bin_t binHandle,
                                          bool* hasConnections)
    {
        return PluginServiceProxyBase::bin_has_connections(pluginService, binHandle, hasConnections);
    }

    sensekit_status_t cycle_bin_buffers(sensekit_bin_t binHandle,
                                        sensekit_frame_t** binBuffer)
    {
        return PluginServiceProxyBase::cycle_bin_buffers(pluginService, binHandle, binBuffer);
    }

    sensekit_status_t link_connection_to_bin(sensekit_streamconnection_t connection,
                                             sensekit_bin_t binHandle)
    {
        return PluginServiceProxyBase::link_connection_to_bin(pluginService, connection, binHandle);
    }

    sensekit_status_t get_parameter_bin(size_t byteSize,
                                        sensekit_parameter_bin_t* binHandle,
                                        sensekit_parameter_data_t* parameterData)
    {
        return PluginServiceProxyBase::get_parameter_bin(pluginService, byteSize, binHandle, parameterData);
    }

    sensekit_status_t log(const char* channel,
                          sensekit_log_severity_t logLevel,
                          const char* format,
                          va_list args)
    {
        return PluginServiceProxyBase::log(pluginService, channel, logLevel, format, args);
    }
    };
}

#endif /* PLUGINSERVICEPROXY_H */
