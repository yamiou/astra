#include "StreamRegistry.h"
#include "Stream.h"
#include <algorithm>

namespace sensekit {

    int operator<(context_id lhs, context_id rhs)
    {
        return lhs.dummy < rhs.dummy;
    }

    StreamRegistry::StreamRegistry() { }
    StreamRegistry::~StreamRegistry() { }

    bool StreamRegistry::register_stream(context_id contextId, Stream* stream)
    {
        if (stream == nullptr)
            return false;

        if (is_stream_registered(stream))
            return false;

        m_streamContextMap.insert(StreamContextMap::value_type(contextId, stream));

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
                               [&stream] (StreamContextMap::value_type& p) {
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
                               [&stream] (StreamContextMap::value_type& p) {
                                   return p.second == stream;
                               });

        return it != m_streamContextMap.end();
    }
}
