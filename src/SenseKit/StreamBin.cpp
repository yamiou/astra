#include "StreamBin.h"

namespace sensekit {

    StreamBin::StreamBin(size_t bufferLengthInBytes)
        : m_frontBufferLocked(ATOMIC_VAR_INIT(false))
    {
        allocate_buffers(bufferLengthInBytes);
    }
    
    CallbackId StreamBin::register_front_buffer_ready_callback(FrontBufferReadyCallback callback)
    {
        return m_frontBufferReadySignal += callback;
    }

    void StreamBin::unregister_front_buffer_ready_callback(CallbackId& callbackId)
    {
        m_frontBufferReadySignal -= callbackId;
        callbackId = 0;
    }

    sensekit_frame_t* StreamBin::get_frontBuffer()
    {
        return m_buffers[m_frontBufferIndex];
    }

    size_t StreamBin::inc_index(size_t index)
    {
        size_t newIndex = index;

        do
        {
            newIndex = newIndex + 1 < BUFFER_COUNT ? newIndex + 1 : 0;
        } while (newIndex == m_frontBufferIndex);

        return newIndex;
    }

    sensekit_frame_t* allocate_frame(size_t bufferLengthInBytes)
    {
        sensekit_frame_t* frame = new sensekit_frame_t;
        frame->byteLength = bufferLengthInBytes;
        frame->data = new uint8_t[bufferLengthInBytes];

        return frame;
    }

    void deallocate_frame(sensekit_frame_t*& frame)
    {
        if (!frame)
            return;

        delete[] (uint8_t*)frame->data;
        delete frame;
        frame = nullptr;
    }

    void StreamBin::allocate_buffers(size_t bufferLengthInBytes)
    {
        for(auto& frame : m_buffers)
        {
            frame = allocate_frame(bufferLengthInBytes);
        }
    }

    sensekit_frame_t* StreamBin::cycle_buffers()
    {
        if (m_frontBufferLocked)
        {
            m_backBufferTailIndex = inc_index(m_backBufferTailIndex);

            if (m_backBufferTailIndex == m_backBufferHeadIndex)
                m_backBufferHeadIndex = inc_index(m_backBufferHeadIndex);
        }
        else
        {
            size_t oldFBI = m_frontBufferIndex;
            m_frontBufferIndex = m_backBufferHeadIndex;
            m_backBufferTailIndex = oldFBI;
            m_backBufferHeadIndex = inc_index(m_backBufferHeadIndex);

            m_frontBufferReadySignal.raise(this);
        }

        // cout << "f: " << m_frontBufferIndex << " b-head: " << m_backBufferHeadIndex
        //      << " b-tail: " << m_backBufferTailIndex << " locked: "
        //      << (m_frontBufferLocked ? "yes" : "no") << endl;

        return m_buffers[m_backBufferTailIndex];
    }

    StreamBin::~StreamBin()
    {
        assert(!has_clients_connected());
        for(auto& frame : m_buffers)
        {
            deallocate_frame(frame);
        }
    }
}
