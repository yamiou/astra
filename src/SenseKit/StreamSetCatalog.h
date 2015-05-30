#ifndef STREAMSETCATALOG_H
#define STREAMSETCATALOG_H

#include <unordered_map>
#include "StreamSet.h"
#include <string>

namespace sensekit {

    class StreamSetCatalog
    {
    public:
        StreamSetCatalog() = default;
        ~StreamSetCatalog();

        StreamSet& get_or_add(std::string uri);
        StreamSet* find_streamset_for_stream(Stream* stream);

        void visit_sets(std::function<void(StreamSet*)> visitorMethod);
        void destroy_set(StreamSet* set);

    private:
        using StreamSetPtr = std::unique_ptr<StreamSet>;
        using StreamSetMap = std::unordered_map<std::string, StreamSetPtr>;

        StreamSetMap m_streamSets;
    };
}

#endif /* STREAMSETCATALOG_H */
