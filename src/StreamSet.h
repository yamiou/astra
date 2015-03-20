#ifndef STREAMSET_H
#define STREAMSET_H

#include "Stream.h"
#include <unordered_map>
#include <vector>

namespace sensekit {

    using StreamSetId = unsigned;
    
    class StreamSet
    {
    public:
        explicit StreamSet(StreamSetId id);
        
        //resolve (type, index) to stream*
        Stream* find_stream(StreamTypeId typeId, int index);
        void add_stream(Stream* stream);
        void remove_stream(Stream* stream);
    private:
        //std::vector<Stream*>
        using StreamMap = std::unordered_map < int, int > ;
        StreamMap m_streamMap;
    };

}

#endif /* STREAMSET_H */