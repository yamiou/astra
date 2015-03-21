#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

#include "Stream.h"

namespace sensekit {

    class StreamBin;

    class StreamConnection
    {
    public:
        StreamConnection(Stream* stream)
            : m_stream(stream)
        { }

        ~StreamConnection();

        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() { return m_bin; }

        StreamHandle get_handle() { return m_handle; }

        Stream* get_stream() { return m_stream; }

    private:
        Stream* m_stream;
        StreamBin* m_bin{nullptr};
        StreamHandle m_handle{0};
    };
}

#endif /* STREAMCONNECTION_H */
