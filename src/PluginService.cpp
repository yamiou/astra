#include "PluginService.h"
#include "Stream.h"
#include "SenseKitContext.h"

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit
{
    StreamSet* PluginService::create_stream_set()
    {
        return m_context.get_setFactory().create();
    }

    sensekit_status_t PluginService::register_stream_added_callback(StreamAddedCallback callback, CallbackId& callbackId)
    {
        callbackId = m_streamAddedSignal += callback;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::register_stream_removed_callback(StreamRemovedCallback callback, CallbackId& callbackId)
    {
        callbackId = m_streamRemovedSignal += callback;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream_added_callback(CallbackId callbackId)
    {
        m_streamAddedSignal -= callbackId;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream_removed_callback(CallbackId callbackId)
    {
        m_streamRemovedSignal -= callbackId;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::register_stream(StreamSetId setId, StreamTypeId typeId, StreamPluginCallbacks pluginCallbacks, StreamHandle& handle)
    {
        StreamId streamId = 0; //TODO assign via factory

        Stream* stream = new Stream(streamId, typeId, 0, pluginCallbacks);

        // TODO add to specific streamset
        m_context.get_rootSet().add_stream(stream);

        handle = stream;

        m_streamAddedSignal.raise(setId, handle, typeId);

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream(StreamHandle& handle)
    {
        Stream* stream = static_cast<Stream*>(handle);

        if (handle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        m_context.get_rootSet().remove_stream(stream);

        m_streamRemovedSignal.raise(m_context.get_rootSet().get_id(), handle, stream->get_typeId());

        delete stream;

        handle = nullptr;

        cout << "unregistered stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream_bin(StreamHandle handle, size_t lengthInBytes,
                                                       StreamBinId& id, sensekit_frame_t*& binBuffer)
    {
        Stream* stream = static_cast<Stream*>(handle);
        StreamBin* bin = stream->create_bin(lengthInBytes);

        id = bin->get_id();
        binBuffer = bin->get_backBuffer();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::destroy_stream_bin(StreamHandle handle, StreamBinId& id, sensekit_frame_t*& buffer)
    {
        Stream* stream = static_cast<Stream*>(handle);
        StreamBin* bin = stream->get_bin_by_id(id);

        stream->destroy_bin(bin);

        id = -1;
        buffer = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::cycle_bin_buffers(StreamHandle handle, StreamBinId id, sensekit_frame_t*& binBuffer)
    {
        Stream* stream = static_cast<Stream*>(handle);
        StreamBin* bin = stream->get_bin_by_id(id);
        binBuffer = bin->cycle_buffers();

        return SENSEKIT_STATUS_SUCCESS;
    }
}