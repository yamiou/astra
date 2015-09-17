#include "StreamSetCatalog.h"
#include <cassert>
#include "StreamSet.h"
#include "StreamSetConnection.h"
#include "Signal.h"
#include "Logger.h"

namespace astra {

    StreamSetCatalog::~StreamSetCatalog() = default;

    StreamSetConnection& StreamSetCatalog::open_set_connection(std::string uri)
    {
        std::string finalUri = uri;
        if (uri == "device/default")
        {
            LOG_INFO("StreamSetCatalog", "default uri provided.");
            finalUri = "device/sensor0";
        }

        StreamSet& set = get_or_add(finalUri, false);
        return *set.add_new_connection();
    }

    void StreamSetCatalog::close_set_connection(StreamSetConnection* conn)
    {
        assert(conn != nullptr);

        StreamSet* set = conn->get_streamSet();
        set->disconnect_streamset_connection(conn);
    }

    StreamSet& StreamSetCatalog::get_or_add(std::string uri, bool claim)
    {
        auto it = m_streamSets.find(uri);

        StreamSet* streamSet;
        if (it == m_streamSets.end())
        {
            StreamSetPtr ssPtr = std::make_unique<StreamSet>(uri);
            streamSet = ssPtr.get();

            auto added = ssPtr->register_for_stream_registered_event(
                [this] (StreamRegisteredEventArgs args)
                {
                    on_stream_registered(args);
                });

            auto removed = ssPtr->register_for_stream_unregistering_event(
                [this] (StreamUnregisteringEventArgs args)
                {
                    on_stream_unregistering(args);
                });

            m_streamSets.insert(std::make_pair(uri, std::make_unique<StreamSetEntry>(std::move(ssPtr), added, removed)));
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

    void StreamSetCatalog::clear()
    {
        m_streamSets.clear();
    }

    void StreamSetCatalog::on_stream_registered(StreamRegisteredEventArgs args)
    {
        m_streamRegisteredSignal.raise(args);
    }

    void StreamSetCatalog::on_stream_unregistering(StreamUnregisteringEventArgs args)
    {
        m_streamUnregisteringSignal.raise(args);
    }

    StreamSet* StreamSetCatalog::find_streamset_for_stream(Stream* stream)
    {
        assert(stream != nullptr);

        auto it = std::find_if(m_streamSets.begin(), m_streamSets.end(),
                               [&stream] (StreamSetMap::value_type& el) -> bool
                               {
                                   return el.second->streamSet->is_member(stream->get_handle());
                               });

        return it != m_streamSets.end() ? it->second->streamSet.get() : nullptr;
    }

    void StreamSetCatalog::visit_sets(std::function<void(StreamSet*)> visitorMethod)
    {
        for(auto& pair : m_streamSets)
        {
            visitorMethod(pair.second->streamSet.get());
        }
    }

    void StreamSetCatalog::destroy_set(astra::StreamSet* set)
    {
        auto it = m_streamSets.find(set->get_uri());

        if (it != m_streamSets.end())
        {
            m_streamSets.erase(it);
        }
    }
}
