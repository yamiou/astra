#include "StreamSet.h"
#include <SenseKitUL/StreamTypes.h>
#include <iostream>
#include <cassert>

namespace sensekit {

    StreamConnection* StreamSet::create_stream_connection(const sensekit_stream_desc_t& desc)
    {
        Stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (stream == nullptr)
        {
            stream = create_stream_placeholder(desc);
        }

        return stream->create_connection();
    }

    bool StreamSet::destroy_stream_connection(StreamConnection* connection)
    {
        assert(connection != nullptr);

        Stream* stream = connection->get_stream();
        stream->destroy_connection(connection);

        if (!stream->has_connections()
            && !stream->is_available())
        {
            m_streamCollection.erase(stream);
            delete stream;
        }

        return true;
    }

    Stream* StreamSet::create_stream(sensekit_stream_desc_t desc,
                                     stream_callbacks_t callbacks)
    {
        Stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (stream == nullptr)
        {
            stream = new Stream(desc);
            m_streamCollection.insert(stream);
        }

        stream->set_callbacks(callbacks);

        return stream;
    }

    Stream* StreamSet::create_stream_placeholder(sensekit_stream_desc_t desc)
    {
        Stream* stream = new Stream(desc);
        m_streamCollection.insert(stream);

        return stream;
    }

    void StreamSet::destroy_stream(Stream* stream)
    {
        assert(stream != nullptr);

        auto it = m_streamCollection.find(stream);
        if (it != m_streamCollection.end())
        {
            (*it)->clear_callbacks();
        }
    }

    bool StreamSet::is_member(sensekit_stream_t streamHandle) const
    {
        assert(streamHandle != nullptr);

        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        return m_streamCollection.find(stream) != m_streamCollection.end();
    }

    sensekit_stream_t StreamSet::find_stream_by_type_subtype(sensekit_stream_type_t type,
                                                             sensekit_stream_subtype_t subtype) const
    {
        Stream* stream = find_stream_by_type_subtype_impl(type, subtype);

        sensekit_stream_t streamHandle = reinterpret_cast<sensekit_stream_t>(stream);
        return streamHandle;
    }

    void StreamSet::visit_streams(std::function<void(Stream*)> visitorMethod)
    {
        for (auto* stream : m_streamCollection)
        {
            visitorMethod(stream);
        }
    }

    Stream* StreamSet::find_stream_by_type_subtype_impl(sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subtype) const
    {
        for (auto* stream : m_streamCollection)
        {
            const sensekit_stream_desc_t& desc = stream->get_description();

            if (desc.type == type && desc.subtype == subtype)
            {
                return stream;
            }
        }

        return nullptr;
    }
}