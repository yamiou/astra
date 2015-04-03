#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

#include "sensekit_types.h"
#include <cassert>

namespace sensekit {

    class StreamBin;
    class Stream;

    class StreamConnection
    {
    public:
        StreamConnection(Stream* stream);
        ~StreamConnection();

        sensekit_frame_ref_t* lock();
        void unlock();

        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() const { return m_bin; }

        StreamHandle* get_handle() const { return m_handle; }

        Stream* get_stream() const { return m_stream; }

        sensekit_streamconnection_t* get_handle() { return m_connection; }
        sensekit_stream_desc_t& get_description() const { return m_connection->desc; }
        size_t get_hash() const;

    private:
        sensekit_streamconnection_t* m_connection{nullptr};
        sensekit_frame_ref_t m_currentFrame;

        bool m_locked{false};
        Stream* m_stream{nullptr};
        StreamBin* m_bin{nullptr};
        StreamHandle* m_handle{nullptr};
    };
}

#endif /* STREAMCONNECTION_H */
