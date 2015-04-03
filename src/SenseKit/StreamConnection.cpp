#include <sensekit_types.h>
#include "StreamConnection.h"
#include "StreamBin.h"
#include "Stream.h"
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    StreamConnection::StreamConnection(Stream* stream)
        : m_stream(stream)
    {
        assert (stream != nullptr);

        m_connection = new sensekit_streamconnection_t;
        m_connection->handle =
            reinterpret_cast<sensekit_streamconnection_handle*>(this);
        m_connection->desc.type = m_stream->get_type();
        m_connection->desc.subType = m_stream->get_subtype();
    }

    size_t StreamConnection::get_hash() const
    {
        assert(m_connection != nullptr);

        std::size_t h1 = std::hash<sensekit_stream_type_t>()(m_connection->desc.type);
        std::size_t h2 = std::hash<sensekit_stream_subtype_t>()(m_connection->desc.subType);

        return h1 ^ (h2 << 1);
    }

    sensekit_frame_ref_t* StreamConnection::lock()
    {
        if (m_locked)
            return &m_currentFrame;

        m_currentFrame.frame = m_bin->lock_front_buffer();
        m_currentFrame.streamConnection = m_connection;

        m_locked = true;

        return &m_currentFrame;
    }

    void StreamConnection::unlock()
    {
        if (!m_locked)
            return;

        m_bin->unlock_front_buffer();
        m_locked = true;
    }

    void StreamConnection::set_bin(StreamBin* new_bin)
    {
        if (m_bin)
        {
            m_bin->dec_ref();
        }

        m_bin = new_bin;

        if (m_bin)
        {
            m_bin->add_ref();
        }
    }

    StreamConnection::~StreamConnection()
    {
        set_bin(nullptr);
        delete m_connection;
    }
}
