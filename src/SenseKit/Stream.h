#ifndef STREAM_H
#define STREAM_H

#include <atomic>
#include <memory>
#include <vector>
#include "Signal.h"
#include "StreamBin.h"
#include "StreamConnection.h"
#include "StreamImpl.h"

namespace sensekit {

    class StreamBin;

    class Stream
    {
    public:
        Stream(StreamImpl* impl)
            : m_impl(impl) {}

        ~Stream() { }

        StreamConnection* create_connection();
        void destroy_connection(StreamConnection* connection);

        void set_parameter(StreamConnection* connection,
                           sensekit_parameter_id id,
                           size_t byteLength,
                           sensekit_parameter_data_t* data);

        void get_parameter_size(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t& byteLength);

        void get_parameter_data(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t byteLength,
                                sensekit_parameter_data_t* data);

        const sensekit_stream_desc_t& get_description() const { return m_impl->get_description(); }

        static Stream* get_ptr(sensekit_stream_t stream)
            { return reinterpret_cast<Stream*>(stream); }

        sensekit_stream_t get_handle()
            { return reinterpret_cast<sensekit_stream_t>(this); }

        StreamImpl* get_impl() { return m_impl.get(); }

    private:
        std::unique_ptr<StreamImpl> m_impl;

        using ConnPtr = std::unique_ptr<StreamConnection>;
        using ConnectionList = std::vector<ConnPtr>;

        ConnectionList m_connections;

    };
}

#endif /* STREAM_H */
