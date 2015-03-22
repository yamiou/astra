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
                return m_buffers[m_backBufferTailIndex];
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
        std::atomic_bool m_frontBufferLocked;

        const static size_t BUFFER_COUNT = 2;
        using FrameBufferArray = std::array<sensekit_frame_t*, BUFFER_COUNT>;

        size_t m_frontBufferIndex{0};
        size_t m_backBufferHeadIndex{1};
        size_t m_backBufferTailIndex{1};

        FrameBufferArray m_buffers;
        int m_refCount{0};

        void allocate_buffers(size_t bufferLengthInBytes);

        sensekit_frame_t* get_frontBuffer()
            {
                return m_buffers[m_frontBufferIndex];
            }

        size_t inc_index(size_t index)
            {
                size_t newIndex = index;

                do
                {
                    newIndex = newIndex + 1 < BUFFER_COUNT ? newIndex + 1 : 0;
                } while (newIndex == m_frontBufferIndex);

                return newIndex;
            }
    };
}

#endif /* STREAMBIN_H */
