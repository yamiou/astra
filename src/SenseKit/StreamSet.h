#ifndef STREAMSET_H
#define STREAMSET_H

#include "Stream.h"

#include <unordered_set>
#include <vector>
#include <memory>

namespace sensekit {

    class StreamSet
    {
    public:
        using StreamListPtr = std::unique_ptr<std::vector<Stream*> >;

        StreamConnection* create_stream_connection(sensekit_stream_desc_t& desc);

        bool destroy_stream_connection(StreamConnection* connection);

        bool has_stream_of_type_subtype(StreamType type, StreamSubtype subtype);

        //plugins only below

        Stream* create_stream(sensekit_stream_desc_t desc, stream_callbacks_t pluginCallbacks);
        void destroy_stream(Stream* stream);

        bool is_member(StreamHandle* stream);

        StreamHandle* find_stream_by_type_subtype(StreamType type, StreamSubtype subtype);

    private:

        Stream* find_stream_by_type_subtype_impl(StreamType type, StreamSubtype subtype);

        using StreamCollection = std::unordered_set<Stream*>;

        StreamCollection m_streamCollection;
    };
}

#endif /* STREAMSET_H */