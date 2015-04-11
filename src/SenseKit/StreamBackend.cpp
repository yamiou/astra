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
            m_bins.erase(it);
    }

    void StreamBackend::on_connection_created(StreamConnection* connection)
    {
        if (m_callbacks &&
            m_callbacks->connectionAddedCallback)
            m_callbacks->connectionAddedCallback(m_callbacks->context,
                                                connection->get_handle());

    }

    void StreamBackend::on_connection_destroyed(StreamConnection* connection)
    {
        if (m_callbacks &&
            m_callbacks->connectionRemovedCallback)
            m_callbacks->connectionRemovedCallback(m_callbacks->context,
                                                  connection->get_handle());
    }

    void StreamBackend::on_set_parameter(StreamConnection* connection,
                                         sensekit_parameter_id id,
                                         size_t byteLength,
                                         sensekit_parameter_data_t* data)
    {

        if (m_callbacks &&
            m_callbacks->setParameterCallback != nullptr)
        {
            m_callbacks->setParameterCallback(m_callbacks->context,
                                             connection->get_handle(),
                                             id,
                                             byteLength,
                                             data);
        }
    }

    void StreamBackend::on_get_parameter_size(StreamConnection* connection,
                                              sensekit_parameter_id id,
                                              size_t& byteLength)
    {
        if (m_callbacks &&
            m_callbacks->getParameterSizeCallback != nullptr)
        {
            m_callbacks->getParameterSizeCallback(m_callbacks->context,
                                                 connection->get_handle(),
                                                 id,
                                                 &byteLength);
        }
    }

    void StreamBackend::on_get_parameter_data(StreamConnection* connection,
                                              sensekit_parameter_id parameterId,
                                              size_t byteLength,
                                              sensekit_parameter_data_t* data)
    {
        sensekit_streamconnection_t* streamConnection =
            reinterpret_cast<sensekit_streamconnection_t*>(connection->get_handle());

        if (m_callbacks &&
            m_callbacks->getParameterDataCallback != nullptr)
        {
            m_callbacks->getParameterDataCallback(m_callbacks->context,
                                                 connection->get_handle(),
                                                 parameterId,
                                                 byteLength,
                                                 data);
        }
    }
}
