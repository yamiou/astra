#ifndef ASTRA_STREAM_BACKEND_H
#define ASTRA_STREAM_BACKEND_H

#include <Astra/astra_types.h>
#include <Astra/Plugins/plugin_capi.h>
#include "astra_stream_connection.hpp"
#include <vector>
#include <memory>

namespace astra {

    class stream_bin;

    class stream_backend
    {
    public:
        stream_backend(astra_stream_desc_t description)
            : m_description(description) {}

        virtual ~stream_backend()
        {
            m_bins.clear();
        }

        stream_bin* create_bin(size_t byteLength);
        void destroy_bin(stream_bin* bin);

        const astra_stream_desc_t& get_description() const { return m_description; }

        bool is_available() { return m_callbacks != nullptr; }

        void set_callbacks(const stream_callbacks_t& callbacks);
        void clear_callbacks();

    protected:
        void on_connection_created(stream_connection* connection, astra_stream_t stream);
        void on_connection_destroyed(stream_connection* connection, astra_stream_t stream);

        void on_connection_started(stream_connection* connection, astra_stream_t stream);
        void on_connection_stopped(stream_connection* connection, astra_stream_t stream);

        void on_set_parameter(stream_connection* connection,
                              astra_parameter_id id,
                              size_t inByteLength,
                              astra_parameter_data_t inData);

        void on_get_parameter(stream_connection* connection,
                              astra_parameter_id id,
                              astra_parameter_bin_t& parameterBin);

        void on_invoke(stream_connection* connection,
                       astra_command_id commandId,
                       size_t inByteLength,
                       astra_parameter_data_t inData,
                       astra_parameter_bin_t& parameterBin);

        virtual void on_destroying_bin(stream_bin* bin) {};

        virtual void on_availability_changed() = 0;

    private:
        using bin_ptr = std::unique_ptr<stream_bin>;
        using bin_list = std::vector<bin_ptr>;

        astra_stream_desc_t m_description;
        std::unique_ptr<stream_callbacks_t> m_callbacks{nullptr};

        bin_list m_bins;
    };
}

#endif /* ASTRA_STREAM_BACKEND_H */
