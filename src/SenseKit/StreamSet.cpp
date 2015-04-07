#include "StreamSet.h"
#include <StreamTypes.h>
#include <iostream>
#include <cassert>

namespace sensekit {

    StreamConnection* StreamSet::create_stream_connection(const sensekit_stream_desc_t& desc)
    {
        Stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subType);

        if (stream == nullptr)
        {
            std::cout << "not found" << std::endl;
            return nullptr;
        }

        return stream->create_connection();
    }

    bool StreamSet::destroy_stream_connection(StreamConnection* connection)
    {
        assert(connection != nullptr);

        Stream* stream = connection->get_stream();
        stream->destroy_connection(connection);

        return true;
    }

    Stream* StreamSet::create_stream(StreamImpl* streamImpl)
    {
        Stream* stream = new Stream(streamImpl);
        m_streamCollection.insert(stream);

        return stream;
    }

    void StreamSet::destroy_stream(Stream* stream)
    {
        assert(stream != nullptr);
        m_streamCollection.erase(m_streamCollection.find(stream));
    }

    bool StreamSet::is_member(sensekit_stream_t streamHandle) const
    {
        assert(streamHandle != nullptr);

        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        return m_streamCollection.find(stream) != m_streamCollection.end();
    }

    sensekit_stream_t StreamSet::find_stream_by_type_subtype(sensekit_stream_type_t type,
                                                             sensekit_stream_subtype_t subType) const
    {
        Stream* stream = find_stream_by_type_subtype_impl(type, subType);

        sensekit_stream_t streamHandle = reinterpret_cast<sensekit_stream_t>(stream);
        return streamHandle;
    }

    Stream* StreamSet::find_stream_by_type_subtype_impl(sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subType) const
    {
        for (auto* stream : m_streamCollection)
        {
            const sensekit_stream_desc_t& desc = stream->get_description();

            if (desc.type == type &&
                (subType == ANY_SUBTYPE || desc.subType == subType))
            {
                return stream;
            }
        }

        return nullptr;
    }
}