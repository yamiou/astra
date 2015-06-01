#include "StreamSet.h"
#include <iostream>
#include <cassert>

namespace sensekit {

    StreamSet::StreamSet(std::string uri)
        : m_logger("StreamSet"),
          m_uri(uri)
    { }

    StreamSet::~StreamSet()
    {}

    StreamConnection* StreamSet::create_stream_connection(const sensekit_stream_desc_t& desc)
    {
        m_logger.info("create_stream_connection for %d, %d", desc.type, desc.subtype);
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
            m_logger.info("create_stream for %d,%d", desc.type, desc.subtype);
            stream = new Stream(desc);
            m_streamCollection.insert(stream);
        }
        else if (stream->is_available())
        {
            m_logger.warn("create_stream for %d,%d already exists, already inflated", desc.type, desc.subtype);
            assert(!stream->is_available());
            return nullptr;
        }
        else
        {
            m_logger.info("create_stream for %d,%d already exists, inflating placeholder stream", desc.type, desc.subtype);
        }

        stream->set_callbacks(callbacks);

        return stream;
    }

    Stream* StreamSet::create_stream_placeholder(sensekit_stream_desc_t desc)
    {
        m_logger.info("create_stream_placeholder for %d,%d", desc.type, desc.subtype);
        Stream* stream = new Stream(desc);
        m_streamCollection.insert(stream);

        return stream;
    }

    StreamSetConnection* StreamSet::add_new_connection()
    {
        StreamSetConnectionPtr ptr = std::make_unique<StreamSetConnection>(this);
        StreamSetConnection* conn = ptr.get();
        m_connections.push_back(std::move(ptr));

        return conn;
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
