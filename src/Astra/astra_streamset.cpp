#include "astra_streamset.hpp"
#include "astra_streamset_connection.hpp"
#include <cassert>

namespace astra {

    streamset::streamset(std::string uri)
        : m_uri(uri)
    { }

    stream_connection* streamset::create_stream_connection(const astra_stream_desc_t& desc)
    {
        LOG_TRACE("astra.streamset", "connecting to (%u,%u) on %s", desc.type,desc.subtype, m_uri.c_str());

        stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (!stream)
        {
            LOG_TRACE("astra.streamset",
                      "registering orphan stream of type (%u,%u) on %s",
                      desc.type,
                      desc.subtype,
                      m_uri.c_str());

            stream = register_orphan_stream(desc);
        }

        return stream->create_connection();
    }

    bool streamset::destroy_stream_connection(stream_connection* connection)
    {
        assert(connection != nullptr);

        if (!connection)
        {
            LOG_WARN("astra.streamset","destroy_stream_connection: attempt to destroy null connection on %s", m_uri.c_str());
            return false;
        }

        stream* stream = connection->get_stream();
        LOG_TRACE("astra.streamset","destroying %p on %s", connection, m_uri.c_str());
        stream->destroy_connection(connection);

        if (!stream->has_connections()
            && !stream->is_available())
        {
            LOG_TRACE("astra.streamset","removing unused/unavailable stream %p on %s",
                      stream,
                      m_uri.c_str());

            m_streamCollection.erase(stream);
            delete stream;
        }

        return true;
    }

    stream* streamset::register_stream(astra_stream_desc_t desc,
                                       stream_callbacks_t callbacks)
    {
        stream* stream = find_stream_by_type_subtype_impl(desc.type, desc.subtype);

        if (stream)
        {
            assert(!stream->is_available());
            if (stream->is_available())
            {
                LOG_WARN("astra.streamset","register_stream: (%u,%u) already exists, already inflated on %s",
                         desc.type,
                         desc.subtype,
                         m_uri.c_str());

                return nullptr;
            }

            LOG_DEBUG("astra.streamset","(%u,%u) already exists, adopting orphan stream on %s",
                      desc.type,
                      desc.subtype,
                      m_uri.c_str());
        }
        else
        {
            LOG_DEBUG("astra.streamset","registering (%u,%u) on %s", desc.type, desc.subtype, m_uri.c_str());
            stream = new astra::stream(desc);
            m_streamCollection.insert(stream);
        }

        stream->set_callbacks(callbacks);

        m_streamRegisteredSignal.raise(stream_registered_event_args(this, stream, desc));

        return stream;
    }

    stream* streamset::register_orphan_stream(astra_stream_desc_t desc)
    {
        LOG_DEBUG("astra.streamset","registering orphan (%u,%u) on %s", desc.type, desc.subtype, m_uri.c_str());
        stream* stream = new astra::stream(desc);
        m_streamCollection.insert(stream);

        return stream;
    }

    streamset_connection* streamset::add_new_connection()
    {
        LOG_TRACE("astra.streamset","new connection to %s", m_uri.c_str());
        streamset_connectionPtr ptr = std::make_unique<streamset_connection>(this);
        streamset_connection* conn = ptr.get();
        m_connections.push_back(std::move(ptr));

        return conn;
    }

    void streamset::disconnect_streamset_connection(streamset_connection* connection)
    {
        assert(connection != nullptr);
        if (!connection)
        {
            LOG_WARN("astra.streamset","disconnect_streamset_connection: null connection on %s", m_uri.c_str());
            return;
        }

        auto it = std::find_if(m_connections.begin(), m_connections.end(),
                               [&connection] (streamset_connectionPtr& ptr) -> bool
                               {
                                   return ptr.get() == connection;
                               });

        if (it != m_connections.end())
        {
            LOG_DEBUG("astra.streamset","disconnecting %p connection from %s",
                      connection,
                      m_uri.c_str());

            m_connections.erase(it);
        }
        else
        {
            LOG_DEBUG("astra.streamset",
                      "disconnect_streamset_connection: %p connection not found on %s",
                      connection,
                      m_uri.c_str());
        }
    }

    void streamset::destroy_stream(stream* stream)
    {
        assert(stream != nullptr);
        if (!stream)
        {
            LOG_WARN("astra.streamset","destroy_stream: null parameter on %s", m_uri.c_str());
        }

        auto it = m_streamCollection.find(stream);
        if (it != m_streamCollection.end())
        {
            LOG_TRACE("astra.streamset","destroying stream %p on %s", stream, m_uri.c_str());
            if (stream->is_available())
            {
                m_streamUnregisteringSignal.raise(
                    stream_unregistering_event_args(this, stream, stream->get_description()));
            }
            stream->clear_callbacks();
        }
        else
        {
            LOG_WARN("astra.streamset","destroy_stream: stream %p not found on %s", stream, m_uri.c_str());
        }
    }

    void streamset::claim()
    {
        assert(m_isClaimed != true);
        if (m_isClaimed)
        {
            LOG_WARN("astra.streamset","claim: %s already claimed", m_uri.c_str());
            return;
        }

        m_isClaimed = true;
    }

    bool streamset::is_claimed() const
    {
        return m_isClaimed;
    }

    astra_callback_id_t streamset::register_for_stream_registered_event(StreamRegisteredCallback callback)
    {
        return m_streamRegisteredSignal += callback;
    }

    astra_callback_id_t streamset::register_for_stream_unregistering_event(StreamUnregisteringCallback callback)
    {
        return m_streamUnregisteringSignal += callback;
    }

    void streamset::unregister_for_stream_registered_event(astra_callback_id_t callbackId)
    {
        m_streamRegisteredSignal -= callbackId;
    }

    void streamset::unregister_for_stream_unregistering_event(astra_callback_id_t callbackId)
    {
        m_streamUnregisteringSignal -= callbackId;
    }

    bool streamset::is_member(astra_stream_t streamHandle) const
    {
        assert(streamHandle != nullptr);

        stream* stream = reinterpret_cast<astra::stream*>(streamHandle);
        return m_streamCollection.find(stream) != m_streamCollection.end();
    }

    astra_stream_t streamset::find_stream_by_type_subtype(astra_stream_type_t type,
                                                          astra_stream_subtype_t subtype) const
    {
        stream* stream = find_stream_by_type_subtype_impl(type, subtype);

        astra_stream_t streamHandle = reinterpret_cast<astra_stream_t>(stream);
        return streamHandle;
    }

    void streamset::visit_streams(std::function<void(stream*)> visitorMethod)
    {
        for (auto* stream : m_streamCollection)
        {
            visitorMethod(stream);
        }
    }

    stream* streamset::find_stream_by_type_subtype_impl(astra_stream_type_t type,
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
