#ifndef STREAMSET_H
#define STREAMSET_H

#include "Stream.h"
#include <unordered_map>
#include <vector>
#include <memory>

namespace sensekit {

    template<typename T>
    class StreamIdHash;

    template<>
    class StreamIdHash<StreamId>
    {
    public:
        size_t operator()(StreamId const& id) const
            {
                return std::hash<unsigned>()(id);
            }
    };

    using StreamSetId = unsigned;

    class StreamSet
    {
    public:
        using StreamListPtr = std::unique_ptr<std::vector<Stream*> >;

        StreamSet(StreamSetId id);

        StreamListPtr find_streams(StreamTypeId typeId);
        void add_stream(Stream* stream);
        bool remove_stream(Stream* stream);
        Stream* get_stream_by_id(StreamId id);

        bool is_member(Stream* stream);

    private:
        using StreamMap = std::unordered_map<StreamId, Stream*, StreamIdHash<StreamId> >;

        const StreamSetId m_id;
        StreamMap m_streamMap;
    };

}

#endif /* STREAMSET_H */