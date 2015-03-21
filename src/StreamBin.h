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

        const static size_t BUFFER_COUNT = 2;
        using FrameBufferArray = std::array<sensekit_frame_t*, BUFFER_COUNT>;

        size_t m_currentBackBufferIndex{0};
        size_t m_currentFrontBufferIndex{BUFFER_COUNT - 1};

        FrameBufferArray m_buffers;
        int m_refCount{0};

        void allocate_buffers(size_t bufferLengthInBytes);
    };
}

#endif /* STREAMBIN_H */
