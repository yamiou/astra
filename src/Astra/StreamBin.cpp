#include "StreamBin.h"
#include <cassert>
#include <Astra/Plugins/plugin_capi.h>

namespace astra {

    StreamBin::StreamBin(size_t bufferLengthInBytes)
        : m_bufferSize(bufferLengthInBytes)
    {
        LOG_TRACE("StreamBin", "Created StreamBin %x", this);
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

    void StreamBin::init_buffer(astra_frame_t& frame, size_t bufferLengthInBytes)
    {
        frame.byteLength = bufferLengthInBytes;
        frame.frameIndex = -1;
        frame.data = new uint8_t[bufferLengthInBytes];
        memset(frame.data, 0, bufferLengthInBytes);
    }

    void StreamBin::deinit_buffer(astra_frame_t& frame)
    {
        uint8_t* data = static_cast<uint8_t*>(frame.data);
        delete[] data;
        frame.data = nullptr;
        frame.frameIndex = -1;
        frame.byteLength = 0;
    }

    astra_callback_id_t StreamBin::register_front_buffer_ready_callback(FrontBufferReadyCallback callback)
    {
        return m_frontBufferReadySignal += callback;
    }

    void StreamBin::unregister_front_buffer_ready_callback(astra_callback_id_t& callbackId)
    {
        m_frontBufferReadySignal -= callbackId;
        callbackId = 0;
    }

    astra_frame_t* StreamBin::get_frontBuffer()
    {
        return &m_buffers[m_frontBufferIndex];
    }

    astra_frame_t* StreamBin::get_middleBuffer()
    {
        return &m_buffers[m_middleBufferIndex];
    }

    astra_frame_t* StreamBin::lock_front_buffer()
    {
        LOG_TRACE("StreamBin", "%x locking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount+1);

        ++m_frontBufferLockCount;
        return get_frontBuffer();
    }

    void StreamBin::unlock_front_buffer()
    {
        LOG_TRACE("StreamBin", "%x unlocking front buffer. lock count: %u -> %u", this, m_frontBufferLockCount, m_frontBufferLockCount-1);
        if (m_frontBufferLockCount == 0)
        {
            //TODO: error, logging
            LOG_WARN("StreamBin", "%x StreamBin unlocked too many times!", this);
            assert(m_frontBufferLockCount != 0);
            return;
        }
        --m_frontBufferLockCount;
        if (m_frontBufferLockCount > 0)
        {
            //can't swap front buffers because there is still an outstanding lock
            return;
        }
        LOG_TRACE("StreamBin", "%x unlock pre indices: f: %d m: %d b: %d",
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


            LOG_TRACE("StreamBin", "%x unlock front/mid indices: f: %d m: %d b: %d",
                this,
                get_frontBuffer()->frameIndex,
                get_middleBuffer()->frameIndex,
                get_backBuffer()->frameIndex);

            raiseFrameReadySignal();
        }
    }

    astra_frame_t* StreamBin::cycle_buffers()
    {
        LOG_TRACE("StreamBin", "%x cycling buffer. lock count: %u produced frame index: %d",
                            this, m_frontBufferLockCount, get_backBuffer()->frameIndex);
        LOG_TRACE("StreamBin", "%x cycle pre indices: f: %d m: %d b: %d",
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

            LOG_TRACE("StreamBin", "%x cycle back/mid indices: f: %d m: %d b: %d",
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
                LOG_WARN("StreamBin", "%x buffers cycled with out-of-order frame indices: %d->%d",
                                this, oldFrameIndex, newFrameIndex);
                assert(frameIndex > oldFrameIndex);
            }
#endif

            LOG_TRACE("StreamBin", "%x cycle front/back indices: f: %d m: %d b: %d",
                this,
                get_frontBuffer()->frameIndex,
                get_middleBuffer()->frameIndex,
                get_backBuffer()->frameIndex);

            raiseFrameReadySignal();
        }

        return get_backBuffer();
    }

    void StreamBin::raiseFrameReadySignal()
    {
        astra_frame_index_t frameIndex = get_frontBuffer()->frameIndex;

        if (frameIndex != -1)
        {
            m_frontBufferReadySignal.raise(this, frameIndex);
        }
    }
}
