#include "StreamSet.h"
#include <sensekit_known_streams.h>

namespace sensekit {
    StreamSet::StreamSet()
    {}

    StreamConnection* StreamSet::open_stream_connection(StreamType type, StreamSubtype subtype)
    {
        Stream* sk_stream = find_stream_by_type_subtype_impl(type, subtype);

        if (sk_stream == nullptr)
        {
            return nullptr;
        }

        return sk_stream->open();
    }

    bool StreamSet::has_stream_of_type_subtype(StreamType type, StreamSubtype subtype)
    {
        return find_stream_by_type_subtype(type, subtype) != nullptr;
    }

    void StreamSet::get_stream_type_subtype(StreamHandle* stream, /*out*/StreamType& type, /*out*/StreamSubtype& subtype)
    {
        Stream* sk_stream = reinterpret_cast<Stream*>(stream);

        type = sk_stream->get_type();
        subtype = sk_stream->get_subtype();
    }

    StreamHandle* StreamSet::create_stream(StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks)
    {
        Stream* sk_stream = new Stream(type, subtype, pluginCallbacks);

        m_streamCollection.insert(sk_stream);

        StreamHandle* stream = reinterpret_cast<StreamHandle*>(sk_stream);
        return stream;
    }

    void StreamSet::destroy_stream(StreamHandle* stream)
    {
        //TODO: check for nullptr
        Stream* sk_stream = reinterpret_cast<Stream*>(stream);

        m_streamCollection.erase(m_streamCollection.find(sk_stream));
    }

    bool StreamSet::is_member(StreamHandle* stream)
    {
        //TODO: check for nullptr
        Stream* sk_stream = reinterpret_cast<Stream*>(stream);

        return m_streamCollection.find(sk_stream) != m_streamCollection.end();
    }

    StreamHandle* StreamSet::find_stream_by_type_subtype(StreamType type, StreamSubtype subtype)
    {
        Stream* sk_stream = find_stream_by_type_subtype_impl(type, subtype);

        StreamHandle* stream = reinterpret_cast<StreamHandle*>(sk_stream);
        return stream;
    }

    Stream* StreamSet::find_stream_by_type_subtype_impl(StreamType type, StreamSubtype subtype)
    {
        for (auto it = m_streamCollection.begin(); it != m_streamCollection.end(); ++it)
        {
            Stream* sk_stream = *it;
            if (sk_stream->get_type() == type && (subtype == ANY_SUBTYPE || sk_stream->get_subtype() == subtype))
            {
                return sk_stream;
            }
        }

        return nullptr;
    }
}