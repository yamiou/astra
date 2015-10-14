#include "astra_stream_bin.hpp"
#include <cassert>
#include <Astra/Plugins/plugin_capi.h>

namespace astra {

    stream_bin::stream_bin(size_t bufferLengthInBytes)
        : m_bufferSize(bufferLengthInBytes)
    {
        LOG_TRACE("stream_bin", "Created stream_bin %x", this);
        init_buffers(bufferLengthInBytes);
    }

    stream_bin::~stream_bin()
    {
        assert(!has_clients_connected());
        deinit_buffers();
    }

    void stream_bin::init_buffers(size_t bufferLengthInBytes)
    {
        for(int i = 0; i < BUFFER_COUNT; ++i)
        {
            init_buffer(m_buffers[i], bufferLengthInBytes);
        }
    }

    void stream_bin::deinit_buffers()
    {
        for(int i = 0; i < BUFFER_COUNT; ++i)
        {
            deinit_buffer(m_buffers[i]);
        }
    }

    void stream_bin::init_buffer(astra_frame_t& frame, size_t bufferLengthInBytes)
    {
        frame.byteLength = bufferLengthInBytes;
        frame.frameIndex = -1;
        frame.data = new uint8_t[bufferLengthInBytes];
        memset(frame.data, 0, bufferLengthInBytes);
    }

    void stream_bin::deinit_buffer(astra_frame_t& frame)
    {
        uint8_t* data = static_cast<uint8_t*>(frame.data);
        delete[] data;
        frame.data = nullptr;
        frame.frameIndex = -1;
        frame.byteLength = 0;
    }

    astra_callback_id_t stream_bin::register_front_buffer_ready_callback(FrontBufferReadyCallback callback)
    {
        return m_frontBufferReadySignal += callback;
    }

    void stream_bin::unregister_front_buffer_ready_callback(astra_callback_id_t& callbackId)
    {
        m_frontBufferReadySignal -= callbackId;
        callbackId = 0;
    }

    astra_frame_t* stream_bin::get_frontBuffer()
    {
        return &m_buffers[m_frontBufferIndex];
    }

    astra_frame_t* stream_bin::get_middleBuffer()
    {
        return &m_buffers[m_middleBufferIndex];
    }

    astra_frame_t* stream_bin::lock_front_buffer()
    {
        LOG_TRACE("stream_bin", "%x locking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount+1);

        ++m_frontBufferLockCount;
        return get_frontBuffer();
    }

    void stream_bin::unlock_front_buffer()
    {
        LOG_TRACE("stream_bin", "%x unlocking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount-1);
        if (m_frontBufferLockCount == 0)
        {
            //TODO: error, logging
            LOG_WARN("stream_bin", "%x stream_bin unlocked too many times!", this);
            assert(m_frontBufferLockCount != 0);
            return;
        }
        --m_frontBufferLockCount;
        if (m_frontBufferLockCount > 0)
        {
            //can't swap front buffers because there is still an outstanding lock
            return;
        }
        LOG_TRACE("stream_bin", "%x unlock pre indices: f: %d m: %d b: %d",
            this,
            get_frontBuffer()->frameIndex,
            get_middleBuffer()->frameIndex,
            get_backBuffer()->frameIndex);
        astra_frame_index_t frontFrameIndex = get_frontBuffer()->frameIndex;
        astra_frame_index_t middleFrameIndex = get_middleBuffer()->frameIndex;

        //if the middle buffer's frameIndex is newer, signal readiness
        if (middleFrameIndex != -1 && middleFrameIndex > frontFrameIndex)
        {
            //back buffer is locked.
            //swap front and middle buffers only
            size_t oldFrontBufferIndex = m_frontBufferIndex;
            m_frontBufferIndex = m_middleBufferIndex;
            m_middleBufferIndex = oldFrontBufferIndex;


            LOG_TRACE("stream_bin", "%x unlock front/mid indices: f: %d m: %d b: %d",
                this,
                get_frontBuffer()->frameIndex,
                get_middleBuffer()->frameIndex,
                get_backBuffer()->frameIndex);

            raiseFrameReadySignal();
        }
    }

    astra_frame_t* stream_bin::cycle_buffers()
    {
        LOG_TRACE("stream_bin", "%x cycling buffer. lock count: %u produced frame index: %d",
                            this, m_frontBufferLockCount, get_backBuffer()->frameIndex);
        LOG_TRACE("stream_bin", "%x cycle pre indices: f: %d m: %d b: %d",
            this,
            get_frontBuffer()->frameIndex,
            get_middleBuffer()->frameIndex,
            get_backBuffer()->frameIndex);
        if (is_front_buffer_locked())
        {
            //Can't change front buffer.
            //swap back and middle buffers only
            size_t oldBackBufferIndex = m_backBufferIndex;
            m_backBufferIndex = m_middleBufferIndex;
            m_middleBufferIndex = oldBackBufferIndex;

            LOG_TRACE("stream_bin", "%x cycle back/mid indices: f: %d m: %d b: %d",
                this,
                get_frontBuffer()->frameIndex,
                get_middleBuffer()->frameIndex,
                get_backBuffer()->frameIndex);
        }
        else
        {
#ifdef DEBUG
            astra_frame_index_t oldFrameIndex = get_frontBuffer()->frameIndex;
#endif
            //The rare case where neither front or back buffers are locked.
            //Rotate back buffer directly to front buffer. (Ignore middle.)
            size_t oldFrontBufferIndex = m_frontBufferIndex;
            m_frontBufferIndex = m_backBufferIndex;
            m_backBufferIndex = oldFrontBufferIndex;

#ifdef DEBUG
            astra_frame_index_t newFrameIndex = get_frontBuffer()->frameIndex;
            if (frameIndex != -1 && newFrameIndex <= oldFrameIndex)
            {
                LOG_WARN("stream_bin", "%x buffers cycled with out-of-order frame indices: %d->%d",
                                this, oldFrameIndex, newFrameIndex);
                assert(frameIndex > oldFrameIndex);
            }
#endif

            LOG_TRACE("stream_bin", "%x cycle front/back indices: f: %d m: %d b: %d",
                this,
                get_frontBuffer()->frameIndex,
                get_middleBuffer()->frameIndex,
                get_backBuffer()->frameIndex);

            raiseFrameReadySignal();
        }

        return get_backBuffer();
    }

    void stream_bin::raiseFrameReadySignal()
    {
        astra_frame_index_t frameIndex = get_frontBuffer()->frameIndex;

        if (frameIndex != -1)
        {
            m_frontBufferReadySignal.raise(this, frameIndex);
        }
    }
}
