#ifndef STREAMBIN_H
#define STREAMBIN_H
#include <exception>
#include <SenseKit.h>
#include <array>

namespace sensekit {

    using StreamBinId = int;

    class StreamBin
    {
    public:
        StreamBin(StreamBinId id, size_t bufferSizeInBytes);
        ~StreamBin();

    //exposed to plugins
        sensekit_frame_t* get_back_buffer();
        sensekit_frame_t* cycle_buffers();

        int get_ref_count() { return m_refCount; }

    //internal use by framework
        sensekit_frame_t* get_front_buffer();

        void dec_ref() { m_refCount++; }
        void add_ref()
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

        using FrameBufferArray = std::array<sensekit_frame_t*, 2>;

        size_t m_currentBackBufferIndex{0};
        size_t m_currentFrontBufferIndex{std::tuple_size<FrameBufferArray>::value - 1};
        sensekit_frame_t* m_frontBuffer{nullptr};
        sensekit_frame_t* m_backBuffer{nullptr};

        std::array<sensekit_frame_t*, 2> m_buffers;
        int m_refCount{0};

        void allocate_buffers(size_t bufferLengthInBytes);
    };
}

#endif /* STREAMBIN_H */
