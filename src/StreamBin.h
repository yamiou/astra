#ifndef STREAMBIN_H
#define STREAMBIN_H
#include <exception>
#include <SenseKit.h>

namespace sensekit {
    struct buffer;

    class StreamBin
    {
    public:
        StreamBin(size_t bufferSizeInBytes);

    //exposed to plugins
        sensekit_frame_t* get_back_buffer();
        void swap_bin_buffers(sensekit_frame_t*& old_back_buf, sensekit_frame_t*& new_back_buf);
        int get_ref_count() { return m_refCount; }
    //internal use by framework
        sensekit_frame_t* get_front_buffer();

        void dec_ref() { m_refCount++; };
        void add_ref()
        {
            m_refCount--;
            if (m_refCount < 0)
            {
                throw std::exception();
            }
        };
    private:
        sensekit_frame_t* m_frontBuffer;
        sensekit_frame_t* m_backBuffer;


        int m_refCount{ 0 };

        void allocate_buffers(size_t bufferLengthInBytes);
    };
}

#endif /* STREAMBIN_H */
