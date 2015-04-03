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

        void start() {};
        void stop() {};

        sensekit_frame_ref_t* lock();
        void unlock();

        void set_bin(StreamBin* new_bin);
        StreamBin* get_bin() const { return m_bin; }

        sensekit_streamconnection_t* get_handle() { return &m_connection; }
        const sensekit_stream_desc_t& get_description() { return m_connection.desc; }

        void set_parameter(sensekit_parameter_id id,
                           size_t byteLength,
                           sensekit_parameter_data_t* data);

        void get_parameter_size(sensekit_parameter_id id,
                                size_t& byteLength);

        void get_parameter_data(sensekit_parameter_id id,
                                size_t byteLength,
                                sensekit_parameter_data_t* data);

    private:
        sensekit_streamconnection_t m_connection;
        sensekit_frame_ref_t m_currentFrame;

        bool m_locked{false};
        Stream* m_stream{nullptr};
        StreamBin* m_bin{nullptr};
        StreamHandle* m_handle{nullptr};
    };
}

#endif /* STREAMCONNECTION_H */
