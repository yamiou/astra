#ifndef STREAM_H
#define STREAM_H

#include <atomic>
#include <memory>
#include <vector>
#include "Signal.h"
#include "StreamConnection.h"
#include "StreamBackend.h"

namespace astra {

    class Stream : public StreamBackend
    {
    public:
        Stream(astra_stream_desc_t description)
            : StreamBackend(description) {}

        virtual ~Stream()
        {
            m_connections.clear();
        }

        Stream& operator=(const Stream& stream) = delete;
        Stream(const Stream& stream) = delete;

        StreamConnection* create_connection();
        void destroy_connection(StreamConnection* connection);

        void start_connection(StreamConnection* connection);
        void stop_connection(StreamConnection* connection);

        void set_parameter(StreamConnection* connection,
                           astra_parameter_id id,
                           size_t inByteLength,
                           astra_parameter_data_t inData);

        void get_parameter(StreamConnection* connection,
                           astra_parameter_id id,
                           astra_parameter_bin_t& parameterBin);

        void invoke(StreamConnection* connection,
                    astra_command_id commandId,
                    size_t inByteLength,
                    astra_parameter_data_t inData,
                    astra_parameter_bin_t& parameterBin);

        astra_stream_t get_handle()
        {
            return reinterpret_cast<astra_stream_t>(this);
        }

        static Stream* get_ptr(astra_stream_t stream)
        {
            return reinterpret_cast<Stream*>(stream);
        }

        virtual void on_availability_changed() override;

        virtual void on_destroying_bin(StreamBin* bin) override
        {
            disconnect_connections(bin);
        }

        bool has_connections() { return m_connections.size() > 0; }
    private:
        void disconnect_connections(StreamBin* bin);

        using ConnPtr = std::unique_ptr<StreamConnection>;
        using ConnectionList = std::vector<ConnPtr>;

        ConnectionList m_connections;
    };
}

#endif /* STREAM_H */
