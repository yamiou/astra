#include "StreamRegistry.h"
#include "Stream.h"
#include "StreamSet.h"
#include <algorithm>

namespace sensekit {

    //int operator<(StreamId lhs, StreamId rhs)
    //{
    //    return lhs.dummy < rhs.dummy;
    //}

    StreamRegistry::StreamRegistry() { }
    StreamRegistry::~StreamRegistry() { }

    bool StreamRegistry::register_stream(Stream* stream)
    {
        if (stream == nullptr)
            return false;

        if (is_stream_registered(stream))
            return false;

        m_streamContextMap.insert(StreamSetMap::value_type(stream->id(), stream));

        raise_registered(stream);

        return true;
    }

    bool StreamRegistry::unregister_stream(Stream* stream)
    {
        if (stream == nullptr)
            return false;

        if (!is_stream_registered(stream))
            return false;

        auto it = std::find_if(m_streamContextMap.begin(), m_streamContextMap.end(),
                               [&stream] (StreamSetMap::value_type& p) {
                                   return p.second == stream;
                               });

        m_streamContextMap.erase(it);

        raise_unregistered(stream);

        return true;
    }

    bool StreamRegistry::is_stream_registered(Stream* stream)
    {
        if (stream == nullptr)
            return false;

        auto it = std::find_if(m_streamContextMap.begin(), m_streamContextMap.end(),
                               [&stream] (StreamSetMap::value_type& p) {
                                   return p.second == stream;
                               });

        return it != m_streamContextMap.end();
    }
}
