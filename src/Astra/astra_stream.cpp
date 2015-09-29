#include <Astra/astra_types.h>
#include "astra_stream.hpp"
#include "astra_stream_bin.hpp"

#include <cstdint>
#include <cassert>
#include <algorithm>

namespace astra {

    void stream::disconnect_connections(stream_bin* bin)
    {
        for (auto& connection : m_connections)
        {
            if (connection->get_bin() == bin)
            {
                connection->set_bin(nullptr);
            }
        }
    }

    stream_connection* stream::create_connection()
    {
        ConnPtr conn(new stream_connection(this));

        stream_connection* rawPtr = conn.get();

        m_connections.push_back(std::move(conn));

        if (is_available())
        {
            on_connection_created(rawPtr, get_handle());
        }

        return rawPtr;
    }

    void stream::destroy_connection(stream_connection* connection)
    {
        auto it = std::find_if(m_connections.cbegin(),
                               m_connections.cend(),
                               [connection] (const std::unique_ptr<stream_connection>& element)
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

    void stream::start_connection(stream_connection* connection)
    {
        if (is_available())
        {
            //TODO check membership
            on_connection_started(connection, get_handle());
        }
    }

    void stream::stop_connection(stream_connection* connection)
    {
        if (is_available())
        {
            //TODO check membership
            on_connection_stopped(connection, get_handle());
        }
    }

    void stream::on_availability_changed()
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

    void stream::set_parameter(stream_connection* connection,
                               astra_parameter_id id,
                               size_t inByteLength,
                               astra_parameter_data_t inData)
    {
        if (is_available())
            on_set_parameter(connection, id, inByteLength, inData);
    }

    void stream::get_parameter(stream_connection* connection,
                               astra_parameter_id id,
                               astra_parameter_bin_t& parameterBin)
    {
        if (is_available())
            on_get_parameter(connection, id, parameterBin);
    }

    void stream::invoke(stream_connection* connection,
                        astra_command_id commandId,
                        size_t inByteLength,
                        astra_parameter_data_t inData,
                        astra_parameter_bin_t& parameterBin)
    {
        if (is_available())
            on_invoke(connection, commandId, inByteLength, inData, parameterBin);
    }
}
