#include "StreamBin.h"
#include <cassert>
#include <SenseKit/Plugins/plugin_capi.h>

namespace sensekit {

    StreamBin::StreamBin(size_t bufferLengthInBytes)
        : m_bufferSize(bufferLengthInBytes),
          m_logger("StreamBin")

    {
        m_logger.trace("Created StreamBin %x", this);
        init_buffers(bufferLengthInBytes);
    }

    StreamBin::~StreamBin()
    {
        assert(!has_clients_connected());
        deinit_buffers();
    }

    void StreamBin::init_buffers(size_t bufferLengthInBytes)
    {
        for(int i = 0; i < BUFFER_COUNT; ++i)
        {
            init_buffer(m_buffers[i], bufferLengthInBytes);
        }
    }

    void StreamBin::deinit_buffers()
    {
        for(int i = 0; i < BUFFER_COUNT; ++i)
        {
            deinit_buffer(m_buffers[i]);
        }
    }

    void StreamBin::init_buffer(sensekit_frame_t& frame, size_t bufferLengthInBytes)
    {
        frame.byteLength = bufferLengthInBytes;
        frame.frameIndex = -1;
        frame.data = new uint8_t[bufferLengthInBytes];
        memset(frame.data, 0, bufferLengthInBytes);
    }

    void StreamBin::deinit_buffer(sensekit_frame_t& frame)
    {
        uint8_t* data = static_cast<uint8_t*>(frame.data);
        delete[] data;
        frame.data = nullptr;
        frame.frameIndex = -1;
        frame.byteLength = 0;
    }

    sensekit_callback_id_t StreamBin::register_front_buffer_ready_callback(FrontBufferReadyCallback callback)
    {
        return m_frontBufferReadySignal += callback;
    }

    void StreamBin::unregister_front_buffer_ready_callback(sensekit_callback_id_t& callbackId)
    {
        m_frontBufferReadySignal -= callbackId;
        callbackId = 0;
    }

    sensekit_frame_t* StreamBin::get_frontBuffer()
    {
        return &m_buffers[m_frontBufferIndex];
    }

    sensekit_frame_t* StreamBin::get_middleBuffer()
    {
        return &m_buffers[m_middleBufferIndex];
    }

    sensekit_frame_t* StreamBin::lock_front_buffer()
    {
        m_logger.trace("%x locking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount+1);

        ++m_frontBufferLockCount;
        return get_frontBuffer();
    }

    void StreamBin::unlock_front_buffer()
    {
        m_logger.trace("%x unlocking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount-1);
        if (m_frontBufferLockCount == 0)
        {
            //TODO: error, logging
            m_logger.warn("%x StreamBin unlocked too many times!", this);
            assert(m_frontBufferLockCount != 0);
            return;
        }
        --m_frontBufferLockCount;
        if (m_frontBufferLockCount > 0)
        {
            //can't swap front buffers because there is still an outstanding lock
            return;
        }

        sensekit_frame_index_t frontFrameIndex = get_frontBuffer()->frameIndex;
        sensekit_frame_index_t middleFrameIndex = get_middleBuffer()->frameIndex;

        //if the middle buffer's frameIndex is newer, signal readiness
        if (middleFrameIndex != -1 && middleFrameIndex > frontFrameIndex)
        {
            //back buffer is locked.
            //swap front and middle buffers only
            size_t oldFrontBufferIndex = m_frontBufferIndex;
            m_frontBufferIndex = m_middleBufferIndex;
            m_middleBufferIndex = oldFrontBufferIndex;

            sensekit_frame_index_t frameIndex = get_frontBuffer()->frameIndex;

            m_frontBufferReadySignal.raise(this, frameIndex);
        }
    }

    sensekit_frame_t* StreamBin::cycle_buffers()
    {
        m_logger.trace("%x cycling buffer. lock count: %u", this, m_frontBufferLockCount);

        if (is_front_buffer_locked())
        {
            //Can't change front buffer.
            //swap back and middle buffers only
            size_t oldBackBufferIndex = m_backBufferIndex;
            m_backBufferIndex = m_middleBufferIndex;
            m_middleBufferIndex = oldBackBufferIndex;
        }
        else
        {
#ifdef DEBUG
            sensekit_frame_index_t oldFrameIndex = get_frontBuffer()->frameIndex;
#endif
            //The rare case where neither front or back buffers are locked.
            //Rotate all three buffers
            size_t oldFrontBufferIndex = m_frontBufferIndex;
            m_frontBufferIndex = m_middleBufferIndex;
            m_middleBufferIndex = m_backBufferIndex;
            m_backBufferIndex = oldFrontBufferIndex;

            sensekit_frame_index_t frameIndex = get_frontBuffer()->frameIndex;

            if (frameIndex != -1)
            {
#ifdef DEBUG
                assert(frameIndex > oldFrameIndex);
#endif
                m_frontBufferReadySignal.raise(this, frameIndex);
            }
        }

        return get_backBuffer();
    }
}
