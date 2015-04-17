#ifndef STREAM_H
#define STREAM_H

#include <atomic>
#include <memory>
#include <vector>
#include "Signal.h"
#include "StreamConnection.h"
#include "StreamBackend.h"

namespace sensekit {

    class Stream : public StreamBackend
    {
    public:

        Stream(sensekit_stream_desc_t description)
            : StreamBackend(description) {}

        virtual ~Stream()
            {
                m_connections.clear();
            }

        StreamConnection* create_connection();
        void destroy_connection(StreamConnection* connection);

        void set_parameter(StreamConnection* connection,
                           sensekit_parameter_id id,
                           size_t inByteLength,
                           sensekit_parameter_data_t inData);

        void get_parameter(StreamConnection* connection,
                           sensekit_parameter_id id,
                           sensekit_parameter_bin_t& parameterBin);

        void invoke(StreamConnection* connection,
                    sensekit_command_id commandId,
                    size_t inByteLength,
                    sensekit_parameter_data_t inData,
                    sensekit_parameter_bin_t& parameterBin);

        sensekit_stream_t get_handle()
            { return reinterpret_cast<sensekit_stream_t>(this); }


        static Stream* get_ptr(sensekit_stream_t stream)
            { return reinterpret_cast<Stream*>(stream); }

        virtual void on_availability_changed() override;

        bool has_connections() { return m_connections.size() > 0; }
    private:

        using ConnPtr = std::unique_ptr<StreamConnection>;
        using ConnectionList = std::vector<ConnPtr>;

        ConnectionList m_connections;
    };
}

#endif /* STREAM_H */
