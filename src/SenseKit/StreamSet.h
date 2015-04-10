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
        StreamConnection* create_stream_connection(const sensekit_stream_desc_t& desc);

        bool destroy_stream_connection(StreamConnection* connection);

        //plugins only below

        Stream* create_stream(sensekit_stream_desc_t desc, stream_callbacks_t callbacks);

        void destroy_stream(Stream* stream);

        bool is_member(sensekit_stream_t stream) const;

        sensekit_stream_t find_stream_by_type_subtype(sensekit_stream_type_t type,
                                                      sensekit_stream_subtype_t subType) const;

        sensekit_streamset_t get_handle()
            { return reinterpret_cast<sensekit_streamset_t>(this); }

        void visit_streams(std::function<void(Stream*)> visitorMethod);

        static StreamSet* get_ptr(sensekit_streamset_t handle)
            { return reinterpret_cast<StreamSet*>(handle); }

    private:
        Stream* find_stream_by_type_subtype_impl(sensekit_stream_type_t type,
                                                 sensekit_stream_subtype_t subType) const;

        using StreamCollection = std::unordered_set<Stream*>;
        StreamCollection m_streamCollection;
    };
}

#endif /* STREAMSET_H */