#include "StreamSet.h"

namespace sensekit {
    StreamSet::StreamSet(StreamSetId id)
        : m_id(id)
    {}

    StreamSet::StreamListPtr StreamSet::find_streams(StreamTypeId typeId)
    {
        StreamSet::StreamListPtr streams(new std::vector<Stream*>());

        auto pred = [typeId] (const StreamMap::value_type& el) -> bool
            {
                return el.second->get_typeId() == typeId;
            };

        for(auto& el : m_streamMap)
        {
            if (pred(el))
                streams->push_back(el.second);
        }

        return streams;
    }

    void StreamSet::add_stream(Stream* stream)
    {
        if (!stream || is_member(stream))
            return;

        m_streamMap.insert(std::make_pair(stream->get_id(), stream));
    }

    bool StreamSet::remove_stream(Stream* stream)
    {
        if (!stream)
            return false;

        return m_streamMap.erase(stream->get_id()) > 0;
    }

    bool StreamSet::is_member(Stream* stream)
    {
        return m_streamMap.find(stream->get_id()) != m_streamMap.end();
    }

    Stream* StreamSet::get_stream_by_id(StreamId id)
    {
        auto it = m_streamMap.find(id);

        return it != m_streamMap.end() ? it->second : nullptr;
    }
}