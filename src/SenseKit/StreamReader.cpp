#include "StreamReader.h"
#include <algorithm>
#include <cassert>

namespace sensekit {
    
    StreamConnection* StreamReader::find_stream_of_type(sensekit_stream_desc_t& desc)
    {
        auto it = m_streamMap.find(desc);

        if (it != m_streamMap.end())
        {
            return it->second;
        }

        return nullptr;
    }

    sensekit_streamconnection_t* StreamReader::get_stream(sensekit_stream_desc_t& desc)
    {
        StreamConnection* connection = find_stream_of_type(desc);

        if (connection != nullptr)
            return connection->get_handle();

        connection = m_streamSet.create_stream_connection(desc);

        if (connection != nullptr)
        {
            m_streamMap.insert(std::make_pair(desc, connection));
        }

        return connection->get_handle();
    }

    sensekit_frame_ref_t* StreamReader::get_subframe(sensekit_stream_desc_t& desc)
    {
        if (!m_locked)
            return nullptr;

        StreamConnection* connection = find_stream_of_type(desc);
        if (connection == nullptr)
            return nullptr;

        return connection->lock();
    }

    CallbackId StreamReader::register_frame_ready_callback(FrameReadyCallback callback)
    {
        return m_frameReadySignal += callback;
    }

    void StreamReader::unregister_frame_ready_callback(CallbackId callbackId)
    {
        m_frameReadySignal -= callbackId;
    }

    void StreamReader::lock()
    {
        if (m_locked)
            return;

        for(auto pair : m_streamMap)
        {
            StreamConnection* connection = pair.second;
            connection->lock();
        }

        m_locked = true;
    }

    void StreamReader::unlock()
    {
        if (!m_locked)
            return; // TODO: exception here?

        for(auto pair : m_streamMap)
        {
            StreamConnection* connection = pair.second;
            connection->unlock();
        }

        m_locked = false;
    }

    bool StreamReader::destroy_stream_connection(StreamConnection* connection)
    {
        assert(connection != nullptr);

        if (m_streamMap.erase(connection->get_description()) > 0)
        {
            m_streamSet.destroy_stream_connection(connection);
            return true;
        }

        return false;
    }

    StreamReader::~StreamReader()
    {
        for(auto pair : m_streamMap)
        {
            m_streamSet.destroy_stream_connection(pair.second);
        }
        m_streamMap.clear();
    }

    void StreamReader::raise_frame_ready()
    {
        bool wasLocked = m_locked;
        if (!wasLocked)
        {
            lock();
        }
        
        m_frameReadySignal.raise(get_handle(), get_handle());

        if (!wasLocked && m_locked)
        {
            unlock();
        }
    }
}
