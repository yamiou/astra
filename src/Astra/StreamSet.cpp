#include "StreamSet.h"
#include "StreamSetConnection.h"
#include <iostream>
#include <cassert>

namespace astra {

    StreamSet::StreamSet(std::string uri)
        : m_uri(uri)
    { }

    StreamConnection* StreamSet::create_stream_connection(const astra_stream_desc_t& desc)
    {
        STRACE("StreamSet", "connecting to (%u,%u) on %s", desc.type,desc.subtype, m_uri.c_str());

        Stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (!stream)
        {
            STRACE("StreamSet",
                   "registering orphan stream of type (%u,%u) on %s",
                   desc.type,
                   desc.subtype,
                   m_uri.c_str());

            stream = register_orphan_stream(desc);
        }

        return stream->create_connection();
    }

    bool StreamSet::destroy_stream_connection(StreamConnection* connection)
    {
        assert(connection != nullptr);

        if (!connection)
        {
            SWARN("StreamSet","destroy_stream_connection: attempt to destroy null connection on %s", m_uri.c_str());
            return false;
        }

        Stream* stream = connection->get_stream();
        STRACE("StreamSet","destroying %p on %s", connection, m_uri.c_str());
        stream->destroy_connection(connection);

        if (!stream->has_connections()
            && !stream->is_available())
        {
            STRACE("StreamSet","removing unused/unavailable stream %p on %s",
                   stream,
                   m_uri.c_str());

            m_streamCollection.erase(stream);
            delete stream;
        }

        return true;
    }

    Stream* StreamSet::register_stream(astra_stream_desc_t desc,
                                       stream_callbacks_t callbacks)
    {
        Stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (stream)
        {
            assert(!stream->is_available());
            if (stream->is_available())
            {
                SWARN("StreamSet","register_stream: (%u,%u) already exists, already inflated on %s",
                      desc.type,
                      desc.subtype,
                      m_uri.c_str());

                return nullptr;
            }

            STRACE("StreamSet","(%u,%u) already exists, adopting orphan stream on %s",
                   desc.type,
                   desc.subtype,
                   m_uri.c_str());
        }
        else
        {
            STRACE("StreamSet","registering (%u,%u) on %s", desc.type, desc.subtype, m_uri.c_str());
            stream = new Stream(desc);
            m_streamCollection.insert(stream);
        }

        stream->set_callbacks(callbacks);

        m_streamRegisteredSignal.raise(StreamRegisteredEventArgs(this, stream, desc));

        return stream;
    }

    Stream* StreamSet::register_orphan_stream(astra_stream_desc_t desc)
    {
        STRACE("StreamSet","registering orphan (%u,%u) on %s", desc.type, desc.subtype, m_uri.c_str());
        Stream* stream = new Stream(desc);
        m_streamCollection.insert(stream);

        return stream;
    }

    StreamSetConnection* StreamSet::add_new_connection()
    {
        STRACE("StreamSet","new connection to %s", m_uri.c_str());
        StreamSetConnectionPtr ptr = std::make_unique<StreamSetConnection>(this);
        StreamSetConnection* conn = ptr.get();
        m_connections.push_back(std::move(ptr));

        return conn;
    }

    void StreamSet::disconnect_streamset_connection(StreamSetConnection* connection)
    {
        assert(connection != nullptr);
        if (!connection)
        {
            SWARN("StreamSet","disconnect_streamset_connection: null connection on %s", m_uri.c_str());
            return;
        }

        auto it = std::find_if(m_connections.begin(), m_connections.end(),
                               [&connection] (StreamSetConnectionPtr& ptr) -> bool
                               {
                                   return ptr.get() == connection;
                               });

        if (it != m_connections.end())
        {
            STRACE("StreamSet","disconnecting %p connection from %s",
                   connection,
                   m_uri.c_str());

            m_connections.erase(it);
        }
        else
        {
            SWARN("StreamSet",
                  "disconnect_streamset_connection: %p connection not found on %s",
                  connection,
                  m_uri.c_str());
        }
    }

    void StreamSet::destroy_stream(Stream* stream)
    {
        assert(stream != nullptr);
        if (!stream)
        {
            SWARN("StreamSet","destroy_stream: null parameter on %s", m_uri.c_str());
        }

        auto it = m_streamCollection.find(stream);
        if (it != m_streamCollection.end())
        {
            STRACE("StreamSet","destroying stream %p on %s", stream, m_uri.c_str());
            if (stream->is_available())
            {
                m_streamUnregisteringSignal.raise(
                    StreamUnregisteringEventArgs(this, stream, stream->get_description()));
            }
            stream->clear_callbacks();
        }
        else
        {
            SWARN("StreamSet","destroy_stream: stream %p not found on %s", stream, m_uri.c_str());
        }
    }

    void StreamSet::claim()
    {
        assert(m_isClaimed != true);
        if (m_isClaimed)
        {
            SWARN("StreamSet","claim: %s already claimed", m_uri.c_str());
            return;
        }

        m_isClaimed = true;
    }

    bool StreamSet::is_claimed() const
    {
        return m_isClaimed;
    }

    astra_callback_id_t StreamSet::register_for_stream_registered_event(StreamRegisteredCallback callback)
    {
        return m_streamRegisteredSignal += callback;
    }

    astra_callback_id_t StreamSet::register_for_stream_unregistering_event(StreamUnregisteringCallback callback)
    {
        return m_streamUnregisteringSignal += callback;
    }

    void StreamSet::unregister_for_stream_registered_event(astra_callback_id_t callbackId)
    {
        m_streamRegisteredSignal -= callbackId;
    }

    void StreamSet::unregister_for_stream_unregistering_event(astra_callback_id_t callbackId)
    {
        m_streamUnregisteringSignal -= callbackId;
    }

    bool StreamSet::is_member(astra_stream_t streamHandle) const
    {
        assert(streamHandle != nullptr);

        Stream* stream = reinterpret_cast<Stream*>(streamHandle);
        return m_streamCollection.find(stream) != m_streamCollection.end();
    }

    astra_stream_t StreamSet::find_stream_by_type_subtype(astra_stream_type_t type,
                                                             astra_stream_subtype_t subtype) const
    {
        Stream* stream = find_stream_by_type_subtype_impl(type, subtype);

        astra_stream_t streamHandle = reinterpret_cast<astra_stream_t>(stream);
        return streamHandle;
    }

    void StreamSet::visit_streams(std::function<void(Stream*)> visitorMethod)
    {
        for (auto* stream : m_streamCollection)
        {
            visitorMethod(stream);
        }
    }

    Stream* StreamSet::find_stream_by_type_subtype_impl(astra_stream_type_t type,
                                                        astra_stream_subtype_t subtype) const
    {
        for (auto* stream : m_streamCollection)
        {
            const astra_stream_desc_t& desc = stream->get_description();

            if (desc.type == type && desc.subtype == subtype)
            {
                return stream;
            }
        }

        return nullptr;
    }
}
