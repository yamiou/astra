#include "PluginService.h"
#include "Stream.h"
#include "SenseKitContext.h"
#include "StreamImpl.h"
#include "PluginServiceDelegate.h"
#include <Plugins/PluginServiceProxyBase.h>
#include <iostream>
#include <sensekit_types.h>
#include "CreatePluginProxy.h"

using std::cout;
using std::endl;

namespace sensekit
{
    PluginServiceProxyBase* PluginService::create_proxy()
    {
        return create_plugin_proxy(this);
    }

    sensekit_status_t PluginService::create_stream_set(sensekit_streamset_t& streamSet)
    {
        //normally would create a new streamset
        StreamSet* actualStreamSet = &m_context.get_rootSet();
        streamSet = actualStreamSet->get_handle();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_set(sensekit_streamset_t& streamSet)
    {
        StreamSet* actualStreamSet = StreamSet::get_ptr(streamSet);

        //if we were not hard coding the rootset in create_stream_set...
        //if streamset has direct child streams, return error
        //if streamset has child streamsets, reparent them to this streamset's parent (or null parent)
        //then delete the streamset

        streamSet = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::register_stream_added_callback(StreamAddedCallback callback,
                                                                    CallbackId& callbackId)
    {
        callbackId = m_streamAddedSignal += callback;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::register_stream_removing_callback(StreamRemovingCallback callback,
                                                                       CallbackId& callbackId)
    {
        callbackId = m_streamRemovingSignal += callback;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream_added_callback(CallbackId callbackId)
    {
        m_streamAddedSignal -= callbackId;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream_removing_callback(CallbackId callbackId)
    {
        m_streamRemovingSignal -= callbackId;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream(sensekit_streamset_t setHandle,
                                                   sensekit_stream_desc_t desc,
                                                   stream_callbacks_t pluginCallbacks,
                                                   sensekit_stream_t& handle)
    {
        // TODO add to specific stream set

        StreamImpl* impl = new StreamImpl(desc, pluginCallbacks);
        Stream* stream = m_context.get_rootSet().create_stream(impl);
        handle = stream->get_handle();

        m_streamAddedSignal.raise(setHandle, handle, desc);

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream(sensekit_stream_t& streamHandle)
    {
        if (streamHandle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        //TODO refactor this mess

        StreamSet* set = &m_context.get_rootSet();
        sensekit_streamset_t setHandle = set->get_handle();

        Stream* stream = Stream::get_ptr(streamHandle);
        const sensekit_stream_desc_t& desc = stream->get_description();

        m_streamRemovingSignal.raise(setHandle, streamHandle, desc);

        set->destroy_stream(stream);

        streamHandle = nullptr;

        cout << "unregistered stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream_bin(sensekit_stream_t streamHandle,
                                                       size_t lengthInBytes,
                                                       sensekit_bin_t& binHandle,
                                                       sensekit_frame_t*& binBuffer)
    {
        Stream* actualStream = Stream::get_ptr(streamHandle);
        StreamImpl* streamImpl = actualStream->get_impl();
        StreamBin* bin = streamImpl->create_bin(lengthInBytes);

        binHandle = bin->get_handle();
        binBuffer = bin->get_backBuffer();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_bin(sensekit_stream_t streamHandle,
                                                        sensekit_bin_t& binHandle,
                                                        sensekit_frame_t*& binBuffer)
    {
        Stream* actualStream = Stream::get_ptr(streamHandle);
        StreamImpl* streamImpl = actualStream->get_impl();

        StreamBin* bin = StreamBin::get_ptr(binHandle);
        streamImpl->destroy_bin(bin);

        binHandle = nullptr;
        binBuffer = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::cycle_bin_buffers(sensekit_bin_t binHandle,
                                                       sensekit_frame_t*& binBuffer)
    {
        StreamBin* bin = StreamBin::get_ptr(binHandle);
        binBuffer = bin->cycle_buffers();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::link_connection_to_bin(sensekit_streamconnection_t* connection,
                                                            sensekit_bin_t binHandle)
    {
        StreamConnection* underlyingConnection = StreamConnection::get_ptr(connection);
        StreamBin* bin = StreamBin::get_ptr(binHandle);

        underlyingConnection->set_bin(bin);

        return SENSEKIT_STATUS_SUCCESS;
    }
}