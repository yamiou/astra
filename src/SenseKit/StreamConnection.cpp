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

        m_connection.handle =
            reinterpret_cast<sensekit_streamconnection_handle*>(this);
        m_connection.desc.type = m_stream->get_type();
        m_connection.desc.subType = m_stream->get_subtype();
    }

    sensekit_frame_ref_t* StreamConnection::lock()
    {
        if (m_locked)
            return &m_currentFrame;

        m_currentFrame.frame = m_bin->lock_front_buffer();
        m_currentFrame.streamConnection = &m_connection;

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

    void StreamConnection::set_parameter(sensekit_parameter_id id,
                                         size_t byteLength,
                                         sensekit_parameter_data_t* data)
    {
        m_stream->set_parameter(this, id, byteLength, data);
    }

    void StreamConnection::get_parameter_size(sensekit_parameter_id id,
                                              size_t& byteLength)
    {
        m_stream->get_parameter_size(this, id, byteLength);
    }

    void StreamConnection::get_parameter_data(sensekit_parameter_id id,
                                              size_t byteLength,
                                              sensekit_parameter_data_t* data)
    {
        m_stream->get_parameter_data(this, id, byteLength, data);
    }

    StreamConnection::~StreamConnection()
    {
        set_bin(nullptr);
    }
}
