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
        return m_context.get_contextFactory().create();
    }

    sensekit_status_t PluginService::register_stream(StreamSetId setId, StreamTypeId typeId, StreamHandle& handle)
    {
        StreamId streamId = 0; //TODO assign via factory
        Stream* stream = new Stream(streamId, typeId, 0);

        //TODO add stream to streamset

        m_stream = stream;
        handle = stream;

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream(StreamHandle& handle)
    {
        Stream* stream = static_cast<Stream*>(handle);

        if (handle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        delete stream;

        handle = nullptr;
        m_stream = nullptr;

        cout << "unregistered stream." << endl;
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::create_stream_bin(StreamHandle handle, unsigned byteLength, StreamBinId& id, sensekit_frame_t*& binBuffer)
    {
        Stream* stream = static_cast<Stream*>(handle);
        StreamBin* bin = stream->create_bin(byteLength);

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

    sensekit_status_t PluginService::cycle_bin_buffers(StreamHandle handle, StreamBinId id, sensekit_frame_t*& new_buf)
    {
        Stream* stream = static_cast<Stream*>(handle);
        StreamBin* bin = stream->get_bin_by_id(id);
        new_buf = bin->cycle_buffers();

        return SENSEKIT_STATUS_SUCCESS;
    }
}