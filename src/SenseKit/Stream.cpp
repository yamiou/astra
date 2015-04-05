#include <sensekit_core.h>
#include "Stream.h"
#include "StreamBin.h"

#include <cstdint>
#include <cassert>
#include <algorithm>

namespace sensekit {

    StreamConnection* Stream::create_connection()
    {
        ConnPtr conn(new StreamConnection(this));

        StreamConnection* rawPtr = conn.get();

        m_connections.push_back(std::move(conn));

        if (m_callbacks.connectionAddedCallback)
            m_callbacks.connectionAddedCallback(m_callbacks.context,
                                                reinterpret_cast<sensekit_streamconnection_t*>(rawPtr));

        return rawPtr;
    }

    void Stream::destroy_connection(StreamConnection* connection)
    {
        auto it = std::find_if(m_connections.cbegin(),
                               m_connections.cend(),
                               [connection] (const std::unique_ptr<StreamConnection>& element)
                               -> bool
                               {
                                   return element.get() == connection;
                               });

        if (it != m_connections.cend())
        {
            if (m_callbacks.connectionRemovedCallback)
                m_callbacks.connectionRemovedCallback(m_callbacks.context,
                                                      reinterpret_cast<sensekit_streamconnection_t*>(connection));

            m_connections.erase(it);
        }
    }

    StreamBin* Stream::create_bin(size_t bufferLengthInBytes)
    {
        BinPtr bin(new StreamBin(bufferLengthInBytes));
        StreamBin* rawPtr = bin.get();

        m_bins.push_back(std::move(bin));

        assert(rawPtr != nullptr);
        return rawPtr;
    }

    void Stream::destroy_bin(StreamBin* bin)
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

    Stream::~Stream()
    {
        m_connections.clear();
        m_bins.clear();
    }

    void Stream::set_parameter(StreamConnection* connection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data)
    {
        sensekit_streamconnection_t* streamConnection = reinterpret_cast<sensekit_streamconnection_t*>(connection);
        if (m_callbacks.setParameterCallback != nullptr)
        {
            m_callbacks.setParameterCallback(m_callbacks.context, streamConnection, id, byteLength, data);
        }
    }

    void Stream::get_parameter_size(StreamConnection* connection, sensekit_parameter_id id, /*out*/size_t& byteLength)
    {
        sensekit_streamconnection_t* streamConnection = reinterpret_cast<sensekit_streamconnection_t*>(connection);
        if (m_callbacks.getParameterSizeCallback != nullptr)
        {
            m_callbacks.getParameterSizeCallback(m_callbacks.context, streamConnection, id, &byteLength);
        }
    }

    void Stream::get_parameter_data(StreamConnection* connection, sensekit_parameter_id parameterId, size_t byteLength, sensekit_parameter_data_t* data)
    {
        sensekit_streamconnection_t* streamConnection = reinterpret_cast<sensekit_streamconnection_t*>(connection);
        if (m_callbacks.getParameterDataCallback != nullptr)
        {
            m_callbacks.getParameterDataCallback(m_callbacks.context, streamConnection, parameterId, byteLength, data);
        }
    }

}
