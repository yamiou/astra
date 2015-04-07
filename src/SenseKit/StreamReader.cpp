#include "StreamReader.h"
#include <algorithm>
#include <cassert>

namespace sensekit {
    using namespace std::placeholders;

    StreamReader::StreamReader(StreamSet& streamSet) :
        m_streamSet(streamSet),
        m_scFrameReadyCallback(nullptr)
    {
    }

    StreamReader::~StreamReader()
    {
        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unregister_frame_ready_callback(data->scFrameReadyCallbackId);
            m_streamSet.destroy_stream_connection(data->connection);
            delete data;
        }
        m_streamMap.clear();
    }

    StreamConnection* StreamReader::find_stream_of_type(sensekit_stream_desc_t& desc)
    {
        auto it = m_streamMap.find(desc);

        if (it != m_streamMap.end())
        {
            return it->second->connection;
        }

        return nullptr;
    }

    SCFrameReadyCallback StreamReader::get_sc_frame_ready_callback()
    {
        if (m_scFrameReadyCallback == nullptr)
        {
            m_scFrameReadyCallback = std::bind(&StreamReader::on_connection_frame_ready, this, _1);
        }
        return m_scFrameReadyCallback;
    }

    sensekit_streamconnection_t* StreamReader::get_stream(sensekit_stream_desc_t& desc)
    {
        StreamConnection* connection = find_stream_of_type(desc);

        if (connection != nullptr)
            return connection->get_handle();

        connection = m_streamSet.create_stream_connection(desc);
        
        if (connection != nullptr)
        {
            CallbackId cbId = connection->register_frame_ready_callback(get_sc_frame_ready_callback());

            ReaderConnectionData* data = new ReaderConnectionData;
            data->connection = connection;
            data->scFrameReadyCallbackId = cbId;
            data->isNewFrameReady = false;

            m_streamMap.insert(std::make_pair(desc, data));

            return connection->get_handle();
        }
        else
        {
            return nullptr;
        }
    }

    void StreamReader::on_connection_frame_ready(StreamConnection* connection)
    {
        auto& desc = connection->get_description();
        
        auto pair = m_streamMap.find(desc);

        if (pair != m_streamMap.end())
        {
            //TODO optimization/special case -- if m_streamMap.size() == 1, call raise_frame_ready() directly
            ReaderConnectionData* data = pair->second;
            data->isNewFrameReady = true;
        }

        check_for_all_frames_ready();
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

    void StreamReader::unregister_frame_ready_callback(CallbackId& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    void StreamReader::lock()
    {
        if (m_locked)
            return;

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->lock();
        }

        m_locked = true;
    }

    void StreamReader::unlock()
    {
        if (!m_locked)
            return; // TODO: exception here?

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unlock();
            data->isNewFrameReady = false;
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

    void StreamReader::check_for_all_frames_ready()
    {
        bool allReady = true;
        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            if (!data->isNewFrameReady)
            {
                //TODO the new frames may not be synced. 
                //We need matching frame indices in the future
                allReady = false;
            }
        }

        if (allReady)
        {
            raise_frame_ready();
        }
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
