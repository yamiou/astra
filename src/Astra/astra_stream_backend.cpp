#include "astra_stream_backend.hpp"
#include "astra_stream_bin.hpp"
#include <algorithm>

namespace astra {

    void stream_backend::set_callbacks(const stream_callbacks_t& callbacks)
    {
        m_callbacks = std::make_unique<stream_callbacks_t>(callbacks);
        on_availability_changed();
    }

    void stream_backend::clear_callbacks()
    {
        m_callbacks.reset();
        on_availability_changed();
    }

    stream_bin* stream_backend::create_bin(size_t bufferLengthInBytes)
    {
        bin_ptr bin(std::make_unique<stream_bin>(bufferLengthInBytes));
        stream_bin* rawPtr = bin.get();

        m_bins.push_back(std::move(bin));

        return rawPtr;
    }

    void stream_backend::destroy_bin(stream_bin* bin)
    {
        auto it = std::find_if(m_bins.cbegin(),
                               m_bins.cend(),
                               [&bin] (const bin_ptr& element)
                               {
                                   return bin == element.get();
                               });



        if (it != m_bins.cend())
        {
            on_destroying_bin(it->get());
            m_bins.erase(it);
        }
    }

    void stream_backend::on_connection_created(stream_connection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_added_callback)
            m_callbacks->connection_added_callback(m_callbacks->context,
                                                   stream,
                                                   connection->get_handle());
    }

    void stream_backend::on_connection_started(stream_connection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_started_callback)
            m_callbacks->connection_started_callback(m_callbacks->context,
                                                     stream,
                                                     connection->get_handle());

    }

    void stream_backend::on_connection_stopped(stream_connection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_stopped_callback)
            m_callbacks->connection_stopped_callback(m_callbacks->context,
                                                     stream,
                                                     connection->get_handle());

    }

    void stream_backend::on_connection_destroyed(stream_connection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_removed_callback)
        {
            astra_bin_t binHandle = connection->get_bin_handle();

            m_callbacks->connection_removed_callback(m_callbacks->context,
                                                     stream,
                                                     binHandle,
                                                     connection->get_handle());
        }
    }

    void stream_backend::on_set_parameter(stream_connection* connection,
                                         astra_parameter_id id,
                                         size_t inByteLength,
                                         astra_parameter_data_t inData)
    {

        if (m_callbacks &&
            m_callbacks->set_parameter_callback != nullptr)
        {
            m_callbacks->set_parameter_callback(m_callbacks->context,
                                                connection->get_handle(),
                                                id,
                                                inByteLength,
                                                inData);
        }
    }

    void stream_backend::on_get_parameter(stream_connection* connection,
                                         astra_parameter_id id,
                                         astra_parameter_bin_t& parameterBin)
    {
        if (m_callbacks &&
            m_callbacks->get_parameter_callback != nullptr)
        {
            m_callbacks->get_parameter_callback(m_callbacks->context,
                                                connection->get_handle(),
                                                id,
                                                &parameterBin);
        }
    }

    void stream_backend::on_invoke(stream_connection* connection,
                                  astra_command_id commandId,
                                  size_t inByteLength,
                                  astra_parameter_data_t inData,
                                  astra_parameter_bin_t& parameterBin)
    {
        if (m_callbacks &&
            m_callbacks->invoke_callback != nullptr)
        {
            m_callbacks->invoke_callback(m_callbacks->context,
                                         connection->get_handle(),
                                         commandId,
                                         inByteLength,
                                         inData,
                                         &parameterBin);
        }
    }
}
