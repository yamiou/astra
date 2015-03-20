#include "StreamBin.h"

namespace sensekit {

    StreamBin::StreamBin(StreamBinId id, size_t bufferLengthInBytes)
        : m_id(id)
    {
        allocate_buffers(bufferLengthInBytes);
    }

    void StreamBin::allocate_buffers(size_t bufferLengthInBytes)
    {
        for(auto* frame : m_buffers)
        {
            frame = new sensekit_frame_t;
            frame->byteLength = bufferLengthInBytes;
            frame->data = new uint8_t[bufferLengthInBytes];
        }
    }

    sensekit_frame_t* StreamBin::get_back_buffer()
    {
        return m_buffers[m_currentBackBufferIndex];
    }

    sensekit_frame_t* StreamBin::cycle_buffers()
    {
        m_currentBackBufferIndex =
            m_currentBackBufferIndex + 1 < m_buffers.size() ? m_currentBackBufferIndex + 1 : 0;

        m_currentFrontBufferIndex =
            m_currentFrontBufferIndex + 1 < m_buffers.size() ? m_currentFrontBufferIndex + 1 : 0;

        return m_buffers[m_currentBackBufferIndex];
    }

    sensekit_frame_t* StreamBin::get_front_buffer()
    {
        return m_buffers[m_currentFrontBufferIndex];
    }

    StreamBin::~StreamBin()
    {
        for(auto* frame : m_buffers)
        {
            delete[] (uint8_t*)frame->data;
            delete frame;
            frame = nullptr;
        }
    }
}
