#include "PluginService.h"
#include "Stream.h"

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit
{
    sensekit_status_t PluginService::register_stream(context_id ctx, stream_type_id id, stream_handle& handle)
    {
        Stream* stream = new Stream(id);

        m_streamRegistry.register_stream(ctx, stream);

        handle = stream;

        cout << "registering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::unregister_stream(stream_handle& handle)
    {
        Stream* stream = static_cast<Stream*>(handle);

        if (handle == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        m_streamRegistry.unregister_stream(stream);

        delete stream;

        handle = nullptr;

        cout << "unregistering stream." << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::orbbec_stream_create_bin(stream_handle handle, unsigned byteLength, bin_id& id, buffer*& new_buf)
    {
        int* bin_data = new int();
        *bin_data = 42;
        id = static_cast<void*>(bin_data);

        //TODO validate byteLength

        if (m_frontBuffer != nullptr)
        {
            delete m_frontBuffer;
        }
        m_backBuffer = new buffer();
        m_backBuffer->byteLength = byteLength;
        m_backBuffer->data = new char[byteLength];

        if (m_frontBuffer != nullptr)
        {
            delete m_frontBuffer;
        }
        m_frontBuffer = new buffer();
        m_frontBuffer->byteLength = byteLength;
        m_frontBuffer->data = new char[byteLength];

        new_buf = m_backBuffer;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::orbbec_stream_destroy_bin(stream_handle handle, bin_id& id, buffer*& old_buf)
    {
        //TODO wat?
        int* bin_data = static_cast<int*>(id);
        delete bin_data;
        bin_data = nullptr;
        id = nullptr;

        if (nullptr != old_buf)
        {
            delete old_buf;
        }
        old_buf = nullptr;

        if (nullptr != m_frontBuffer)
        {
            delete m_frontBuffer;
        }
        m_frontBuffer = nullptr;
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t PluginService::orbbec_swap_bin_buffer(stream_handle handle, buffer*& old_buf, buffer*& new_buf)
    {
        //TODO do we need to pass in old_buf? even if only for developer mental model?
        //TODO assert old_buf != new_buf, or old_buf is the designated backbuffer
        buffer* temp = m_frontBuffer;
        m_frontBuffer = m_backBuffer;
        m_backBuffer = temp;

        new_buf = m_backBuffer;

        return SENSEKIT_STATUS_SUCCESS;
    }
}