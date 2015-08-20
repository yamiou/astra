#include "PluginServiceImpl.h"
#include "Stream.h"
#include "StreamSetCatalog.h"
#include <Astra/astra_types.h>
#include "StreamRegisteredEventArgs.h"
#include "StreamUnregisteringEventArgs.h"
#include "ParameterBin.h"
#include "Logging.h"
#include <cstdio>
#include <memory>

namespace astra
{
    void PluginServiceImpl::notify_host_event(astra_event_id id, const void* data, size_t dataSize)
    {
        m_hostEventSignal.raise(id, data, dataSize);
    }

    astra_status_t PluginServiceImpl::create_stream_set(const char* streamUri, astra_streamset_t& streamSet)
    {
        StreamSet& set = m_setCatalog.get_or_add(streamUri, true);
        streamSet = set.get_handle();

        SINFO("PluginService", "creating streamset: %s %x", streamUri, streamSet);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::destroy_stream_set(astra_streamset_t& streamSet)
    {
        StreamSet* actualSet = StreamSet::get_ptr(streamSet);

        SINFO("PluginService", "destroying streamset: %s %x", actualSet->get_uri().c_str(), streamSet);
        m_setCatalog.destroy_set(actualSet);

        streamSet = nullptr;

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::register_stream_registered_callback(stream_registered_callback_t callback,
                                                                    void* clientTag,
                                                                    CallbackId& callbackId)
    {
        auto thunk = [clientTag, callback](StreamRegisteredEventArgs args)
            {
                callback(clientTag,
                         args.streamSet->get_handle(),
                         args.stream->get_handle(),
                         args.description);
            };

        callbackId = m_setCatalog.register_for_stream_registered_event(thunk);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::register_stream_unregistering_callback(stream_unregistering_callback_t callback,
                                                                       void* clientTag,
                                                                       CallbackId& callbackId)
    {
        auto thunk = [clientTag, callback](StreamUnregisteringEventArgs args)
            {
                callback(clientTag,
                         args.streamSet->get_handle(),
                         args.stream->get_handle(),
                         args.description);
            };

        callbackId = m_setCatalog.register_for_stream_unregistering_event(thunk);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::unregister_stream_registered_callback(CallbackId callbackId)
    {
        m_setCatalog.unregister_for_stream_registered_event(callbackId);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::unregister_stream_unregistering_callback(CallbackId callbackId)
    {
        m_setCatalog.unregister_form_stream_unregistering_event(callbackId);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::create_stream(astra_streamset_t setHandle,
                                                   astra_stream_desc_t desc,
                                                   stream_callbacks_t pluginCallbacks,
                                                   astra_stream_t& handle)
    {
        // TODO add to specific stream set
        StreamSet* set = StreamSet::get_ptr(setHandle);
        Stream* stream = set->register_stream(desc, pluginCallbacks);
        handle = stream->get_handle();

        SINFO("PluginService", "registered stream -- handle %x type: %d", handle, desc.type);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::destroy_stream(astra_stream_t& streamHandle)
    {
        if (streamHandle == nullptr)
            return ASTRA_STATUS_INVALID_PARAMETER;

        Stream* stream = Stream::get_ptr(streamHandle);

        assert(stream != nullptr);

        StreamSet* set =
            m_setCatalog.find_streamset_for_stream(stream);

        assert(set != nullptr);

        const astra_stream_desc_t& desc = stream->get_description();

        SINFO("PluginService", "destroying stream -- handle: %x type: %d", stream->get_handle(), desc.type);

        set->destroy_stream(stream);

        streamHandle = nullptr;

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::get_streamset_uri(astra_streamset_t setHandle,
                                                       const char*& uri)
    {
        assert(setHandle != nullptr);

        StreamSet* actualSet = StreamSet::get_ptr(setHandle);
        uri = actualSet->get_uri().c_str();

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::create_stream_bin(astra_stream_t streamHandle,
                                                       size_t lengthInBytes,
                                                       astra_bin_t& binHandle,
                                                       astra_frame_t*& binBuffer)
    {
        Stream* actualStream = Stream::get_ptr(streamHandle);
        StreamBin* bin = actualStream->create_bin(lengthInBytes);

        binHandle = bin->get_handle();
        binBuffer = bin->get_backBuffer();

        SINFO("PluginService", "creating bin -- handle: %x stream: %x type: %d size: %u",
                      binHandle,
                      streamHandle,
                      actualStream->get_description().type,
                      lengthInBytes);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::destroy_stream_bin(astra_stream_t streamHandle,
                                                        astra_bin_t& binHandle,
                                                        astra_frame_t*& binBuffer)
    {
        Stream* actualStream = Stream::get_ptr(streamHandle);
        StreamBin* bin = StreamBin::get_ptr(binHandle);

        SINFO("PluginService", "destroying bin -- %x stream: %x type: %d size: %u",
                      binHandle,
                      streamHandle,
                      actualStream->get_description().type,
                      bin->bufferSize());

        actualStream->destroy_bin(bin);

        binHandle = nullptr;
        binBuffer = nullptr;

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::bin_has_connections(astra_bin_t binHandle, bool& hasConnections)
    {
        StreamBin* bin = StreamBin::get_ptr(binHandle);
        hasConnections = bin->has_clients_connected();

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::cycle_bin_buffers(astra_bin_t binHandle,
                                                       astra_frame_t*& binBuffer)
    {
        assert(binHandle != nullptr);

        StreamBin* bin = StreamBin::get_ptr(binHandle);
        binBuffer = bin->cycle_buffers();

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::link_connection_to_bin(astra_streamconnection_t connection,
                                                            astra_bin_t binHandle)
    {
        StreamConnection* underlyingConnection = StreamConnection::get_ptr(connection);
        StreamBin* bin = StreamBin::get_ptr(binHandle);

        Stream* stream = underlyingConnection->get_stream();
        if (binHandle != nullptr)
        {
            SINFO("PluginService", "linking connection to bin -- stream: %x type: %d conn: %x bin: %x",
                          stream->get_handle(),
                          stream->get_description().type,
                          connection,
                          bin);
        }
        else
        {
            SINFO("PluginService", "unlinking connection to bin -- stream: %x type: %d conn: %x",
                          stream->get_handle(),
                          stream->get_description().type,
                          connection);
        }

        underlyingConnection->set_bin(bin);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::get_parameter_bin(size_t byteSize,
                                                       astra_parameter_bin_t& binHandle,
                                                       astra_parameter_data_t& parameterData)
    {
        //TODO pooling
        ParameterBin* parameterBin = new ParameterBin(byteSize);

        binHandle = parameterBin->get_handle();
        parameterData = parameterBin->data();

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::log(const char* channel,
                              astra_log_severity_t logLevel,
                              const char* fileName,
                              int lineNo,
                              const char* func,
                              const char* format,
                              va_list args)
    {
        astra::log_vargs(channel, logLevel, fileName, lineNo, func, format, args);
        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::register_host_event_callback(host_event_callback_t callback,
                                                                  void* clientTag,
                                                                  CallbackId& callbackId)
    {
        auto thunk = [clientTag, callback](astra_event_id id, const void* data, size_t dataSize)
            {
                callback(clientTag, id, data, dataSize);
            };

        callbackId = m_hostEventSignal += thunk;

        //m_context.raise_existing_device_unavailable(callback, clientTag);

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t PluginServiceImpl::unregister_host_event_callback(CallbackId callbackId)
    {
        m_hostEventSignal -= callbackId;

        return ASTRA_STATUS_SUCCESS;
    }
}
