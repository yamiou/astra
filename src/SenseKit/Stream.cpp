#include <SenseKit/sensekit_types.h>
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

        if (is_available())
        {
            on_connection_created(rawPtr, get_handle());
        }

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
            if (is_available())
            {
                on_connection_destroyed(it->get(), get_handle());
            }
            m_connections.erase(it);
        }
    }

    void Stream::on_availability_changed()
    {
        if (is_available())
        {
            for(auto& connection : m_connections)
            {
                on_connection_created(connection.get(), get_handle());
            }
        }
        else
        {
            for(auto& connection : m_connections)
            {
                connection->set_bin(nullptr);
            }
        }
    }

    void Stream::set_parameter(StreamConnection* connection,
                               sensekit_parameter_id id,
                               size_t inByteLength,
                               sensekit_parameter_data_t inData)
    {
        if (is_available())
            on_set_parameter(connection, id, inByteLength, inData);
    }

    void Stream::get_parameter(StreamConnection* connection, 
                               sensekit_parameter_id id,
                               sensekit_parameter_bin_t& parameterBin)
    {
        if (is_available())
            on_get_parameter(connection, id, parameterBin);
    }

    void Stream::invoke(StreamConnection* connection, 
                        sensekit_command_id commandId, 
                        size_t inByteLength, 
                        sensekit_parameter_data_t inData,
                        sensekit_parameter_bin_t& parameterBin)
    {
        if (is_available())
            on_invoke(connection, commandId, inByteLength, inData, parameterBin);
    }
}
