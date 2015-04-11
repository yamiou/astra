#include <sensekit_types.h>
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
            on_connection_created(rawPtr);
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
              on_connection_destroyed(it->get());
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
                on_connection_created(connection.get());
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
                               size_t byteLength,
                               sensekit_parameter_data_t* data)
    {
        if (is_available())
            on_set_parameter(connection, id, byteLength, data);
    }

    void Stream::get_parameter_size(StreamConnection* connection,
                                    sensekit_parameter_id id,
                                    size_t& byteLength)
    {
        if (is_available())
            on_get_parameter_size(connection, id, byteLength);
    }

    void Stream::get_parameter_data(StreamConnection* connection,
                                    sensekit_parameter_id parameterId,
                                    size_t byteLength,
                                    sensekit_parameter_data_t* data)
    {
        if (is_available())
            on_get_parameter_data(connection, parameterId, byteLength, data);
    }
}
