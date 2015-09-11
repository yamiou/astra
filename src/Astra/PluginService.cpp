#include "PluginService.h"
#include "PluginServiceImpl.h"
#include "CreatePluginProxy.h"

namespace astra
{
    PluginService::PluginService(StreamSetCatalog& catalog)
        : m_impl(std::make_unique<PluginServiceImpl>(catalog)),
          m_proxy(create_plugin_proxy(this))
    {}

    PluginService::~PluginService()
    {}

    void PluginService::notify_host_event(astra_event_id id, const void* data, size_t dataSize)
    {
        m_impl->notify_host_event(id, data, dataSize);
    }

    PluginServiceProxyBase* PluginService::proxy()
    {
        return m_proxy.get();
    }

   astra_status_t PluginService::register_stream_registered_callback(stream_registered_callback_t callback,
                                                                     void* clientTag,
                                                                     astra_callback_id_t& callbackId)
   {
       return m_impl->register_stream_registered_callback(callback, clientTag, callbackId);
   }

   astra_status_t PluginService::register_stream_unregistering_callback(stream_unregistering_callback_t callback,
                                                                        void* clientTag,
                                                                        astra_callback_id_t& callbackId)
   {
       return m_impl->register_stream_unregistering_callback(callback, clientTag, callbackId);
   }

   astra_status_t PluginService::register_host_event_callback(host_event_callback_t callback,
                                                              void* clientTag,
                                                              astra_callback_id_t& callbackId)
   {
       return m_impl->register_host_event_callback(callback, clientTag, callbackId);
   }

   astra_status_t PluginService::unregister_host_event_callback(astra_callback_id_t callback)
   {
       return m_impl->unregister_host_event_callback(callback);
   }

   astra_status_t PluginService::unregister_stream_registered_callback(astra_callback_id_t callback)
   {
       return m_impl->unregister_stream_registered_callback(callback);
   }

   astra_status_t PluginService::unregister_stream_unregistering_callback(astra_callback_id_t callback)
   {
       return m_impl->unregister_stream_unregistering_callback(callback);
   }

   astra_status_t PluginService::create_stream_set(const char* setUri,
                                                   astra_streamset_t& setHandle)
   {
       return m_impl->create_stream_set(setUri, setHandle);
   }

   astra_status_t PluginService::destroy_stream_set(astra_streamset_t& setHandle)
   {
       return m_impl->destroy_stream_set(setHandle);
   }

   astra_status_t PluginService::get_streamset_uri(astra_streamset_t setHandle,
                                                   const char*& uri)
   {
       return m_impl->get_streamset_uri(setHandle, uri);
   }

   astra_status_t PluginService::create_stream(astra_streamset_t setHandle,
                                               astra_stream_desc_t desc,
                                               stream_callbacks_t pluginCallbacks,
                                               astra_stream_t& handle)
   {
       return m_impl->create_stream(setHandle, desc, pluginCallbacks, handle);
   }

   astra_status_t PluginService::destroy_stream(astra_stream_t& handle)
   {
       return m_impl->destroy_stream(handle);
   }

   astra_status_t PluginService::create_stream_bin(astra_stream_t streamHandle,
                                                   size_t lengthInBytes,
                                                   astra_bin_t& binHandle,
                                                   astra_frame_t*& binBuffer)
   {
       return m_impl->create_stream_bin(streamHandle, lengthInBytes, binHandle, binBuffer);
   }

   astra_status_t PluginService::destroy_stream_bin(astra_stream_t streamHandle,
                                                    astra_bin_t& binHandle,
                                                    astra_frame_t*& binBuffer)
   {
       return m_impl->destroy_stream_bin(streamHandle, binHandle, binBuffer);
   }

   astra_status_t PluginService::bin_has_connections(astra_bin_t binHandle,
                                                     bool& hasConnections)
   {
       return m_impl->bin_has_connections(binHandle, hasConnections);
   }

   astra_status_t PluginService::cycle_bin_buffers(astra_bin_t binHandle,
                                                   astra_frame_t*& binBuffer)
   {
       return m_impl->cycle_bin_buffers(binHandle, binBuffer);
   }

   astra_status_t PluginService::link_connection_to_bin(astra_streamconnection_t connection,
                                                        astra_bin_t binHandle)
   {
       return m_impl->link_connection_to_bin(connection, binHandle);
   }

   astra_status_t PluginService::get_parameter_bin(size_t byteSize,
                                                   astra_parameter_bin_t& binHandle,
                                                   astra_parameter_data_t& parameterData)
   {
       return m_impl->get_parameter_bin(byteSize, binHandle, parameterData);
   }

   astra_status_t PluginService::log(const char* channel,
                                     astra_log_severity_t logLevel,
                                     const char* fileName,
                                     int lineNo,
                                     const char* func,
                                     const char* format,
                                     va_list args)
   {
       return m_impl->log(channel, logLevel, fileName, lineNo, func, format, args);
   }


}
