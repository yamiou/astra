#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

#include "Stream.h"

namespace sensekit {

    using StreamHandle = unsigned;
    class StreamBin;

    class StreamConnection
    {
    public:
        explicit StreamConnection(const Stream* stream)
            : m_stream(stream)
        {
            
        }

        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() { return m_bin; }

        StreamHandle get_handle() { return m_handle; }

        const Stream* get_stream()  { return m_stream; }
    private:
        const Stream* m_stream;
        StreamBin* m_bin;
        StreamHandle m_handle;
    };
}

#endif /* STREAMCONNECTION_H */
