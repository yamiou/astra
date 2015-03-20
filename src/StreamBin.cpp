#include "StreamBin.h"

namespace sensekit {

    StreamBin::StreamBin(size_t bufferLengthInBytes)
    {
        allocate_buffers(bufferLengthInBytes);
    }

    void StreamBin::allocate_buffers(size_t bufferLengthInBytes)
    {
        if (m_frontBuffer != nullptr)
        {
            delete m_frontBuffer;
        }

        m_backBuffer = new sensekit_frame_t;
        m_backBuffer->byteLength = bufferLengthInBytes;
        m_backBuffer->data = new uint8_t[bufferLengthInBytes];

        if (m_frontBuffer != nullptr)
        {
            delete m_frontBuffer;
        }

        m_frontBuffer = new sensekit_frame_t;
        m_frontBuffer->byteLength = bufferLengthInBytes;
        m_frontBuffer->data = new uint8_t[bufferLengthInBytes];
    }


    sensekit_frame_t* StreamBin::get_back_buffer()
    {
        return m_backBuffer;
    }

    void StreamBin::swap_bin_buffers(sensekit_frame_t*& old_back_buf, sensekit_frame_t*& new_back_buf)
    {

    }

    sensekit_frame_t* StreamBin::get_front_buffer()
    {
        return m_frontBuffer;
    }

    StreamBin::~StreamBin()
    {
        if (m_frontBuffer)
        {
            delete[] (uint8_t*)m_frontBuffer->data;
            delete m_frontBuffer;
        }

        if (m_backBuffer)
        {
            delete[] (uint8_t*)m_backBuffer->data;
            delete m_backBuffer;
        }
    }
}
