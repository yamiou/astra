#include "astra_streamset_catalog.hpp"
#include <cassert>
#include "astra_streamset.hpp"
#include "astra_streamset_connection.hpp"
#include "astra_signal.hpp"
#include "astra_logger.hpp"

namespace astra {

    streamset_catalog::~streamset_catalog() = default;

    streamset_connection& streamset_catalog::open_set_connection(std::string uri)
    {
        std::string finalUri = uri;
        if (uri == "device/default")
        {
            LOG_INFO("streamset_catalog", "default uri provided.");
            finalUri = "device/sensor0";
        }

        streamset& set = get_or_add(finalUri, false);
        return *set.add_new_connection();
    }

    void streamset_catalog::close_set_connection(streamset_connection* conn)
    {
        assert(conn != nullptr);

        streamset* set = conn->get_streamSet();
        set->disconnect_streamset_connection(conn);
    }

    streamset& streamset_catalog::get_or_add(std::string uri, bool claim)
    {
        auto it = m_streamSets.find(uri);

        streamset* streamSet;
        if (it == m_streamSets.end())
        {
            streamset_ptr ssPtr = std::make_unique<streamset>(uri);
            streamSet = ssPtr.get();

            auto added = ssPtr->register_for_stream_registered_event(
                [this] (stream_registered_event_args args)
                {
                    on_stream_registered(args);
                });

            auto removed = ssPtr->register_for_stream_unregistering_event(
                [this] (stream_unregistering_event_args args)
                {
                    on_stream_unregistering(args);
                });

            m_streamSets.insert(std::make_pair(uri, std::make_unique<streamset_entry>(std::move(ssPtr), added, removed)));
        }
        else
        {
            streamSet = it->second->streamSet.get();
        }

        if (claim)
        {
            streamSet->claim();
        }

        return *streamSet;
    }

    void streamset_catalog::clear()
    {
        m_streamSets.clear();
    }

    void streamset_catalog::on_stream_registered(stream_registered_event_args args)
    {
        m_streamRegisteredSignal.raise(args);
    }

    void streamset_catalog::on_stream_unregistering(stream_unregistering_event_args args)
    {
        m_streamUnregisteringSignal.raise(args);
    }

    streamset* streamset_catalog::find_streamset_for_stream(stream* stream)
    {
        assert(stream != nullptr);

        auto it = std::find_if(m_streamSets.begin(), m_streamSets.end(),
                               [&stream] (streamsetMap::value_type& el) -> bool
                               {
                                   return el.second->streamSet->is_member(stream->get_handle());
                               });

        return it != m_streamSets.end() ? it->second->streamSet.get() : nullptr;
    }

    void streamset_catalog::visit_sets(std::function<void(streamset*)> visitorMethod)
    {
        for(auto& pair : m_streamSets)
        {
            visitorMethod(pair.second->streamSet.get());
        }
    }

    void streamset_catalog::destroy_set(astra::streamset* set)
    {
        auto it = m_streamSets.find(set->get_uri());

        if (it != m_streamSets.end())
        {
            m_streamSets.erase(it);
        }
    }
}
