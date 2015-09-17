#include "StreamBackend.h"
#include "StreamBin.h"
#include <algorithm>

namespace astra {

    StreamBin* StreamBackend::create_bin(size_t bufferLengthInBytes)
    {
        BinPtr bin(std::make_unique<StreamBin>(bufferLengthInBytes));
        StreamBin* rawPtr = bin.get();

        m_bins.push_back(std::move(bin));

        return rawPtr;
    }

    void StreamBackend::destroy_bin(StreamBin* bin)
    {
        auto it = std::find_if(m_bins.cbegin(),
                               m_bins.cend(),
                               [&bin] (const BinPtr& element)
                               {
                                   return bin == element.get();
                               });



        if (it != m_bins.cend())
        {
            on_destroying_bin(it->get());
            m_bins.erase(it);
        }
    }

    void StreamBackend::on_connection_created(StreamConnection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_added_callback)
            m_callbacks->connection_added_callback(m_callbacks->context,
                                                   stream,
                                                   connection->get_handle());
    }

    void StreamBackend::on_connection_started(StreamConnection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_started_callback)
            m_callbacks->connection_started_callback(m_callbacks->context,
                                                     stream,
                                                     connection->get_handle());

    }

    void StreamBackend::on_connection_stopped(StreamConnection* connection, astra_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_stopped_callback)
            m_callbacks->connection_stopped_callback(m_callbacks->context,
                                                     stream,
                                                     connection->get_handle());

    }

    void StreamBackend::on_connection_destroyed(StreamConnection* connection, astra_stream_t stream)
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

    void StreamBackend::on_set_parameter(StreamConnection* connection,
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

    void StreamBackend::on_get_parameter(StreamConnection* connection,
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

    void StreamBackend::on_invoke(StreamConnection* connection,
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
