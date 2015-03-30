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

        StreamSet();

        StreamConnection* open_stream_connection(StreamType type, StreamSubtype subtype);

        bool has_stream_of_type_subtype(StreamType type, StreamSubtype subtype);

        void get_stream_type_subtype(StreamHandle* stream, /*out*/StreamType& type, /*out*/StreamSubtype& subtype);

        //plugins only below

        StreamHandle* create_stream(StreamType type, StreamSubtype subtype, stream_callbacks_t pluginCallbacks);
        void destroy_stream(StreamHandle* stream);

        bool is_member(StreamHandle* stream);

        StreamHandle* find_stream_by_type_subtype(StreamType type, StreamSubtype subtype);

    private:

        Stream* find_stream_by_type_subtype_impl(StreamType type, StreamSubtype subtype);

        using StreamCollection = std::unordered_set<Stream*>;

        StreamCollection m_streamCollection;
    };

}

#endif /* STREAMSET_H */