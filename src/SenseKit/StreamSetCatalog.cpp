#include "StreamSetCatalog.h"
#include <cassert>
#include "StreamSet.h"
#include "StreamSetConnection.h"

namespace sensekit {

    StreamSetCatalog::~StreamSetCatalog()
    {
        m_streamSets.clear();
    }

    StreamSetConnection& StreamSetCatalog::open_set_connection(std::string uri)
    {
        std::string finalUri = uri;
        if (uri == "device/default")
        {
            m_logger.info("default uri provided.");
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
            m_streamSets.insert(std::make_pair(uri, std::move(ssPtr)));
        }
        else
        {
            streamSet = it->second.get();
        }

        if (claim)
        {
            streamSet->claim();
        }

        return *streamSet;
    }

    StreamSet* StreamSetCatalog::find_streamset_for_stream(Stream* stream)
    {
        assert(stream != nullptr);

        auto it = std::find_if(m_streamSets.begin(), m_streamSets.end(),
                               [&stream] (StreamSetMap::value_type& el) -> bool
                               {
                                   return el.second->is_member(stream->get_handle());
                               });

        return it != m_streamSets.end() ? it->second.get() : nullptr;
    }

    void StreamSetCatalog::visit_sets(std::function<void(StreamSet*)> visitorMethod)
    {
        for(auto& pair : m_streamSets)
        {
            visitorMethod(pair.second.get());
        }
    }

    void StreamSetCatalog::destroy_set(sensekit::StreamSet* set)
    {
        auto it = m_streamSets.find(set->get_uri());

        if (it != m_streamSets.end())
        {
            m_streamSets.erase(it);
        }
    }
}
