#include "StreamReader.h"
#include <algorithm>

namespace sensekit {

    StreamConnection* StreamReader::find_stream_of_type(sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subType)
    {
        auto it = std::find_if(m_streamConnections.cbegin(),
                               m_streamConnections.cend(),
                               [&type,&subType] (const StreamConnection* connection)
                               -> bool
                               {
                                   Stream* underlyingStream = connection->get_stream();
                                   return underlyingStream->get_type() == type &&
                                       underlyingStream->get_subtype() == subType;
                               });

        if (it != m_streamConnections.end())
        {
            return *it;
        }

        return nullptr;
    }

    StreamConnection* StreamReader::get_stream(sensekit_stream_type_t type,
                                               sensekit_stream_subtype_t subType)
    {
        StreamConnection* connection = find_stream_of_type(type, subType);

        if (connection != nullptr)
            return connection;

        connection = m_streamSet.open_stream_connection(type, subType);
        if (connection != nullptr)
        {
            m_streamConnections.insert(connection);
        }

        return connection;
    }

    bool StreamReader::close_stream(StreamConnection* connection)
    {
        if (m_streamConnections.erase(connection) > 0)
        {
            m_streamSet.close_stream_connection(connection);
            return true;
        }

        return false;
    }
}
