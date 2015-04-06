#include "PluginService.h"
#include "Stream.h"
#include "SenseKitContext.h"
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

    sensekit_status_t PluginService::create_stream_set(sensekit_streamset_t*& streamset)
    {
        //normally would create a new streamset
        StreamSet* skStreamSet = &m_context.get_rootSet();
        streamset = reinterpret_cast<sensekit_streamset_t*>(skStreamSet);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_set(sensekit_streamset_t*& streamset)
    {
        StreamSet* skStreamSet = reinterpret_cast<StreamSet*>(streamset);

        //if we were not hard coding the rootset in create_stream_set...
        //if streamset has direct child streams, return error
        //if streamset has child streamsets, reparent them to this streamset's parent (or null parent)
        //then delete the streamset

        streamset = nullptr;

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

    sensekit_status_t PluginService::create_stream(StreamSetHandle* setHandle,
                                                   sensekit_stream_desc_t desc,
                                                   stream_callbacks_t pluginCallbacks,
                                                   sensekit_stream_handle_t& handle)
    {
        // TODO add to specific streamset
        Stream* stream = m_context.get_rootSet().create_stream(desc, pluginCallbacks);
        handle = reinterpret_cast<sensekit_stream_handle_t>(stream);

        m_streamAddedSignal.raise(setHandle, handle, desc);

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream(sensekit_stream_handle_t& streamHandle)
    {
        if (streamHandle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        //TODO refactor this mess

        StreamSet* set = &m_context.get_rootSet();

        StreamSetHandle* setHandle = reinterpret_cast<StreamSetHandle*>(set);

        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        const sensekit_stream_desc_t& desc = stream->get_description();

        m_streamRemovingSignal.raise(setHandle, streamHandle, desc);

        set->destroy_stream(stream);

        streamHandle = nullptr;

        cout << "unregistered stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream_bin(sensekit_stream_handle_t streamHandle,
                                                       size_t lengthInBytes,
                                                       sensekit_bin_handle_t& binHandle,
                                                       sensekit_frame_t*& binBuffer)
    {
        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        StreamBin* bin = stream->create_bin(lengthInBytes);

        binHandle = reinterpret_cast<sensekit_bin_handle_t>(bin);
        binBuffer = bin->get_backBuffer();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_bin(sensekit_stream_handle_t streamHandle,
                                                        sensekit_bin_handle_t& binHandle,
                                                        sensekit_frame_t*& binBuffer)
    {
        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        StreamBin* bin = reinterpret_cast<StreamBin*>(binHandle);

        stream->destroy_bin(bin);

        binHandle = nullptr;
        binBuffer = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::cycle_bin_buffers(sensekit_bin_handle_t binHandle,
                                                       sensekit_frame_t*& binBuffer)
    {
        StreamBin* bin = reinterpret_cast<StreamBin*>(binHandle);
        binBuffer = bin->cycle_buffers();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::link_connection_to_bin(sensekit_streamconnection_t* connection,
                                                            sensekit_bin_handle_t binHandle)
    {
        StreamConnection* underlyingConnection = reinterpret_cast<StreamConnection*>(connection->handle);
        StreamBin* bin = reinterpret_cast<StreamBin*>(binHandle);

        underlyingConnection->set_bin(bin);

        return SENSEKIT_STATUS_SUCCESS;
    }
}