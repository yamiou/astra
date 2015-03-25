#include "PluginService.h"
#include "Stream.h"
#include "SenseKitContext.h"

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit
{
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

    sensekit_status_t PluginService::register_stream_added_callback(StreamAddedCallback callback, CallbackId& callbackId)
    {
        callbackId = m_streamAddedSignal += callback;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::register_stream_removing_callback(StreamRemovingCallback callback, CallbackId& callbackId)
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

    sensekit_status_t PluginService::create_stream(StreamSetHandle* setHandle, StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks, /*out*/StreamHandle*& handle)
    {
        // TODO add to specific streamset
        StreamHandle* stream = m_context.get_rootSet().create_stream(type, subtype, pluginCallbacks);

        handle = stream;

        m_streamAddedSignal.raise(setHandle, handle, type, subtype);

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream(StreamHandle*& streamHandle)
    {
        if (streamHandle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        StreamSet* set = &m_context.get_rootSet();

        StreamSetHandle* setHandle = reinterpret_cast<StreamSetHandle*>(set);
        StreamType type;
        StreamSubtype subtype;
        set->get_stream_type_subtype(streamHandle, type, subtype);
        m_streamRemovingSignal.raise(setHandle, streamHandle, type, subtype);

        set->destroy_stream(streamHandle);

        streamHandle = nullptr;

        cout << "unregistered stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream_bin(StreamHandle* handle, size_t lengthInBytes,
                                                       StreamBinId& id, sensekit_frame_t*& binBuffer)
    {
        Stream* stream = reinterpret_cast<Stream*>(handle);
        StreamBin* bin = stream->create_bin(lengthInBytes);

        id = bin->get_id();
        binBuffer = bin->get_backBuffer();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_bin(StreamHandle* handle, StreamBinId& id, sensekit_frame_t*& buffer)
    {
        Stream* stream = reinterpret_cast<Stream*>(handle);
        StreamBin* bin = stream->get_bin_by_id(id);

        stream->destroy_bin(bin);

        id = -1;
        buffer = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::cycle_bin_buffers(StreamHandle* handle, StreamBinId id, sensekit_frame_t*& binBuffer)
    {
        Stream* stream = reinterpret_cast<Stream*>(handle);
        StreamBin* bin = stream->get_bin_by_id(id);
        binBuffer = bin->cycle_buffers();

        return SENSEKIT_STATUS_SUCCESS;
    }
}