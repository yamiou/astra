#ifndef STREAMBIN_H
#define STREAMBIN_H

#include <exception>
#include <SenseKit.h>
#include <array>
#include <atomic>

namespace sensekit {

    using StreamBinId = bin_id_t;

    class StreamBin
    {
    public:
        StreamBin(StreamBinId id, size_t bufferSizeInBytes);
        ~StreamBin();

        //exposed to plugins
        sensekit_frame_t* get_backBuffer()
            {
                return m_buffers[m_currentBackBufferIndex];
            }

        sensekit_frame_t* cycle_buffers();

        //TODO support reference counting for multi-client scenarios
        sensekit_frame_t* lock_front_buffer()
            {
                m_frontBufferLocked = true;
                return get_frontBuffer();
            }

        void unlock_front_buffer()
            {
                m_frontBufferLocked = false;
            }

        int get_ref_count() { return m_refCount; }

        void add_ref() { m_refCount++; }
        void dec_ref()
            {
                m_refCount--;
                if (m_refCount < 0)
                {
                    throw std::exception();
                }
            }

        StreamBinId get_id() { return m_id; }

    private:
        StreamBinId m_id{-1};
        std::atomic_bool m_frontBufferLocked{ATOMIC_VAR_INIT(false)};

        const static size_t BUFFER_COUNT = 2;
        using FrameBufferArray = std::array<sensekit_frame_t*, BUFFER_COUNT>;

        size_t m_currentBackBufferIndex{0};
        size_t m_currentFrontBufferIndex{BUFFER_COUNT - 1};

        FrameBufferArray m_buffers;
        int m_refCount{0};

        void allocate_buffers(size_t bufferLengthInBytes);

        sensekit_frame_t* get_frontBuffer()
            {
                return m_buffers[m_currentFrontBufferIndex];
            }
    };
}

#endif /* STREAMBIN_H */
