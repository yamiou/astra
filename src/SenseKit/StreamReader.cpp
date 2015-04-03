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

        connection = m_streamSet.open_stream_connection(desc.type,
                                                        desc.subType);
        if (connection != nullptr)
        {
            m_streamMap.insert(std::make_pair(desc, connection));
        }

        return connection->get_handle();;
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

    bool StreamReader::close_stream(StreamConnection* connection)
    {
        assert(connection != nullptr);

        if (m_streamMap.erase(connection->get_description()) > 0)
        {
            m_streamSet.close_stream_connection(connection);
            return true;
        }

        return false;
    }
}
