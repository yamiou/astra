#ifndef STREAMBACKEND_H
#define STREAMBACKEND_H

#include <SenseKit/sensekit_types.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include "StreamConnection.h"
#include <vector>
#include <memory>

namespace sensekit {

    class StreamBin;

    class StreamBackend
    {
    public:
        StreamBackend(sensekit_stream_desc_t description)
            : m_description(description) {}

        virtual ~StreamBackend()
        {
            m_bins.clear();
        }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        const sensekit_stream_desc_t& get_description() const { return m_description; }

        bool is_available() { return m_callbacks != nullptr; }

        void set_callbacks(const stream_callbacks_t& callbacks)
        {
            m_callbacks = std::make_unique<stream_callbacks_t>(callbacks);
            on_availability_changed();
        }

        void clear_callbacks()
        {
            m_callbacks.reset();
            on_availability_changed();
        }

    protected:
        void on_connection_created(StreamConnection* connection, sensekit_stream_t stream);
        void on_connection_destroyed(StreamConnection* connection, sensekit_stream_t stream);

        void on_set_parameter(StreamConnection* connection,
                              sensekit_parameter_id id,
                              size_t inByteLength,
                              sensekit_parameter_data_t inData);

        void on_get_parameter(StreamConnection* connection,
                              sensekit_parameter_id id,
                              sensekit_parameter_bin_t& parameterBin);

        void on_invoke(StreamConnection* connection,
                       sensekit_command_id commandId,
                       size_t inByteLength,
                       sensekit_parameter_data_t inData,
                       sensekit_parameter_bin_t& parameterBin);

        virtual void on_availability_changed() = 0;

    private:
        using BinPtr = std::unique_ptr<StreamBin>;
        using BinList = std::vector<BinPtr>;

        sensekit_stream_desc_t m_description;
        std::unique_ptr<stream_callbacks_t> m_callbacks{nullptr};

        BinList m_bins;
    };
}

#endif /* STREAMBACKEND_H */
