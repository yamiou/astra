#include "StreamSetCatalog.h"
#include <cassert>

namespace sensekit {

    StreamSetCatalog::~StreamSetCatalog()
    {
        m_streamSets.clear();
    }

    StreamSet& StreamSetCatalog::get_or_add(std::string uri)
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

        if (it != m_streamSets.end())
        {
            return it->second.get();
        }
        else
        {
            return nullptr;
        }
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
