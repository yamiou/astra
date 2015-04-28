#include "StreamBackend.h"
#include "StreamBin.h"

namespace sensekit {

    StreamBin* StreamBackend::create_bin(size_t bufferLengthInBytes)
    {
        BinPtr bin(new StreamBin(bufferLengthInBytes));
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

    void StreamBackend::on_connection_created(StreamConnection* connection, sensekit_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_added_callback)
            m_callbacks->connection_added_callback(m_callbacks->context,
                                                   stream,
                                                   connection->get_handle());

    }

    void StreamBackend::on_connection_destroyed(StreamConnection* connection, sensekit_stream_t stream)
    {
        if (m_callbacks &&
            m_callbacks->connection_removed_callback)
        {
            sensekit_bin_t binHandle = connection->get_bin_handle();

            m_callbacks->connection_removed_callback(m_callbacks->context,
                                                     stream,
                                                     binHandle,
                                                     connection->get_handle());
        }
    }

    void StreamBackend::on_set_parameter(StreamConnection* connection,
                                         sensekit_parameter_id id,
                                         size_t inByteLength,
                                         sensekit_parameter_data_t inData)
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
                                         sensekit_parameter_id id,
                                         sensekit_parameter_bin_t& parameterBin)
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
                                  sensekit_command_id commandId,
                                  size_t inByteLength,
                                  sensekit_parameter_data_t inData,
                                  sensekit_parameter_bin_t& parameterBin)
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
