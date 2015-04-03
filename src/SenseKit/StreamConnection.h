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

        void unlock(sensekit_frame_ref_t* frameRef);

        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() const { return m_bin; }

        StreamHandle* get_handle() const { return m_handle; }

        Stream* get_stream() const { return m_stream; }

        sensekit_streamconnection_t* get_handle() { return m_connection; }

    private:
        sensekit_streamconnection_t* m_connection{nullptr};
        Stream* m_stream;
        StreamBin* m_bin{nullptr};
        StreamHandle* m_handle{nullptr};
    };
}

#endif /* STREAMCONNECTION_H */
