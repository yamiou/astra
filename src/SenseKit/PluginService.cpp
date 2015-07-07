#include "PluginService.h"
#include "PluginServiceImpl.h"
#include "CreatePluginProxy.h"

namespace sensekit
{
    PluginService::PluginService(StreamSetCatalog& catalog)
        : m_impl(std::make_unique<PluginServiceImpl>(catalog)),
          m_proxy(create_plugin_proxy(this))
    {}

    PluginService::~PluginService()
    {}

    void PluginService::notify_host_event(sensekit_event_id id, const void* data, size_t dataSize)
    {
        notify_host_event(id, data, dataSize);
    }

    PluginServiceProxyBase* PluginService::proxy()
    {
        return m_proxy.get();
    }

   sensekit_status_t PluginService::register_stream_registered_callback(stream_registered_callback_t callback,
                                                                        void* clientTag,
                                                                        sensekit_callback_id_t& callbackId)
   {
       return m_impl->register_stream_registered_callback(callback, clientTag, callbackId);
   }

   sensekit_status_t PluginService::register_stream_unregistering_callback(stream_unregistering_callback_t callback,
                                                                           void* clientTag,
                                                                           sensekit_callback_id_t& callbackId)
   {
       return m_impl->register_stream_unregistering_callback(callback, clientTag, callbackId);
   }

   sensekit_status_t PluginService::register_host_event_callback(host_event_callback_t callback,
                                                                 void* clientTag,
                                                                 sensekit_callback_id_t& callbackId)
   {
       return m_impl->register_host_event_callback(callback, clientTag, callbackId);
   }

   sensekit_status_t PluginService::unregister_host_event_callback(sensekit_callback_id_t callback)
   {
       return m_impl->unregister_host_event_callback(callback);
   }

   sensekit_status_t PluginService::unregister_stream_registered_callback(sensekit_callback_id_t callback)
   {
       return m_impl->unregister_stream_registered_callback(callback);
   }

   sensekit_status_t PluginService::unregister_stream_unregistering_callback(sensekit_callback_id_t callback)
   {
       return m_impl->unregister_stream_unregistering_callback(callback);
   }

   sensekit_status_t PluginService::create_stream_set(const char* setUri,
                                                      sensekit_streamset_t& setHandle)
   {
       return m_impl->create_stream_set(setUri, setHandle);
   }

   sensekit_status_t PluginService::destroy_stream_set(sensekit_streamset_t& setHandle)
   {
       return m_impl->destroy_stream_set(setHandle);
   }

   sensekit_status_t PluginService::get_streamset_uri(sensekit_streamset_t setHandle,
                                                      const char*& uri)
   {
       return m_impl->get_streamset_uri(setHandle, uri);
   }

   sensekit_status_t PluginService::create_stream(sensekit_streamset_t setHandle,
                                                  sensekit_stream_desc_t desc,
                                                  stream_callbacks_t pluginCallbacks,
                                                  sensekit_stream_t& handle)
   {
       return m_impl->create_stream(setHandle, desc, pluginCallbacks, handle);
   }

   sensekit_status_t PluginService::destroy_stream(sensekit_stream_t& handle)
   {
       return m_impl->destroy_stream(handle);
   }

   sensekit_status_t PluginService::create_stream_bin(sensekit_stream_t streamHandle,
                                                      size_t lengthInBytes,
                                                      sensekit_bin_t& binHandle,
                                                      sensekit_frame_t*& binBuffer)
   {
       return m_impl->create_stream_bin(streamHandle, lengthInBytes, binHandle, binBuffer);
   }

   sensekit_status_t PluginService::destroy_stream_bin(sensekit_stream_t streamHandle,
                                                       sensekit_bin_t& binHandle,
                                                       sensekit_frame_t*& binBuffer)
   {
       return m_impl->destroy_stream_bin(streamHandle, binHandle, binBuffer);
   }

   sensekit_status_t PluginService::bin_has_connections(sensekit_bin_t binHandle,
                                                        bool& hasConnections)
   {
       return m_impl->bin_has_connections(binHandle, hasConnections);
   }

   sensekit_status_t PluginService::cycle_bin_buffers(sensekit_bin_t binHandle,
                                                      sensekit_frame_t*& binBuffer)
   {
       return m_impl->cycle_bin_buffers(binHandle, binBuffer);
   }

   sensekit_status_t PluginService::link_connection_to_bin(sensekit_streamconnection_t connection,
                                                           sensekit_bin_t binHandle)
   {
       return m_impl->link_connection_to_bin(connection, binHandle);
   }

   sensekit_status_t PluginService::get_parameter_bin(size_t byteSize,
                                                      sensekit_parameter_bin_t& binHandle,
                                                      sensekit_parameter_data_t& parameterData)
   {
       return m_impl->get_parameter_bin(byteSize, binHandle, parameterData);
   }

   sensekit_status_t PluginService::log(const char* channel,
                                        sensekit_log_severity_t logLevel,
                                        const char* fileName,
                                        int lineNo,
                                        const char* func,
                                        const char* format,
                                        va_list args)
   {
       return m_impl->log(channel, logLevel, fileName, lineNo, func, format, args);
   }


}
