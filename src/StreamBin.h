#ifndef STREAMBIN_H
#define STREAMBIN_H
#include <exception>

namespace sensekit {
    struct buffer;

    class StreamBin
    {
    public:
    //exposed to plugins
        buffer* get_back_buffer();
        void swap_bin_buffers(buffer*& old_back_buf, buffer*& new_back_buf);
        int get_ref_count() { return m_refCount; }
    //internal use by framework
        buffer* get_front_buffer();

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
        int m_refCount{ 0 };
    };
}

#endif /* STREAMBIN_H */
