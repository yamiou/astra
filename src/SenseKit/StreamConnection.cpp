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

    sensekit_frame_ref_t* StreamConnection::lock()
    {
        sensekit_frame_ref_t* frameRef = new sensekit_frame_ref_t;
        frameRef->frame = m_bin->lock_front_buffer();
        frameRef->streamConnection = m_connection;

        return frameRef;
    }

    void StreamConnection::unlock(sensekit_frame_ref_t* frameRef)
    {
        assert(frameRef != nullptr);
        m_bin->unlock_front_buffer();

        delete frameRef;
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
