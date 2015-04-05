#include "StreamSet.h"
#include <StreamTypes.h>
#include <iostream>

namespace sensekit {

    StreamConnection* StreamSet::create_stream_connection(sensekit_stream_desc_t& desc)
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
        Stream* stream = connection->get_stream();
        stream->destroy_connection(connection);

        return true;
    }

    bool StreamSet::has_stream_of_type_subtype(StreamType type, StreamSubtype subtype)
    {
        return find_stream_by_type_subtype(type, subtype) != nullptr;
    }

    Stream* StreamSet::create_stream(sensekit_stream_desc_t desc, stream_callbacks_t pluginCallbacks)
    {
        Stream* sk_stream = new Stream(desc, pluginCallbacks);

        m_streamCollection.insert(sk_stream);

        return sk_stream;
    }

    void StreamSet::destroy_stream(Stream* stream)
    {
        //TODO: check for nullptr

        m_streamCollection.erase(m_streamCollection.find(stream));
    }

    bool StreamSet::is_member(StreamHandle* stream)
    {
        //TODO: check for nullptr
        Stream* sk_stream = reinterpret_cast<Stream*>(stream);

        return m_streamCollection.find(sk_stream) != m_streamCollection.end();
    }

    StreamHandle* StreamSet::find_stream_by_type_subtype(StreamType type, StreamSubtype subtype)
    {
        Stream* sk_stream = find_stream_by_type_subtype_impl(type, subtype);

        StreamHandle* stream = reinterpret_cast<StreamHandle*>(sk_stream);
        return stream;
    }

    Stream* StreamSet::find_stream_by_type_subtype_impl(StreamType type, StreamSubtype subtype)
    {
        for (auto it = m_streamCollection.begin(); it != m_streamCollection.end(); ++it)
        {
            Stream* sk_stream = *it;
            if (sk_stream->get_type() == type && (subtype == DEFAULT_SUBTYPE || sk_stream->get_subtype() == subtype))
            {
                return sk_stream;
            }
        }

        return nullptr;
    }
}