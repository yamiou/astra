#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

#include "sensekit_types.h"

namespace sensekit {

    class StreamBin;
    class Stream;

    class StreamConnection
    {
    public:
        StreamConnection(Stream* stream)
            : m_stream(stream)
        {
            //TODO throw if stream is nullptr
        }

        ~StreamConnection();

        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() const { return m_bin; }

        StreamHandle* get_handle() const { return m_handle; }

        Stream* get_stream() const { return m_stream; }
    private:
        Stream* m_stream;
        StreamBin* m_bin{nullptr};
        StreamHandle* m_handle{nullptr};
    };
}

#endif /* STREAMCONNECTION_H */
