#include "StreamBin.h"

//#include <iostream>
//using std::cout;
//using std::endl;

namespace sensekit {

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

    StreamBin::StreamBin(StreamBinId id, size_t bufferLengthInBytes)
        : m_id(id)
    {
        allocate_buffers(bufferLengthInBytes);
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
        }

        // cout << "f: " << m_frontBufferIndex << " b-head: " << m_backBufferHeadIndex
        //      << " b-tail: " << m_backBufferTailIndex << " locked: "
        //      << (m_frontBufferLocked ? "yes" : "no") << endl;

        return m_buffers[m_backBufferTailIndex];
    }

    StreamBin::~StreamBin()
    {
        for(auto& frame : m_buffers)
        {
            deallocate_frame(frame);
        }
    }
}
