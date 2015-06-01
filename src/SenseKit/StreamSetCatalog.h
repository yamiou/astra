#ifndef STREAMSETCATALOG_H
#define STREAMSETCATALOG_H

#include <unordered_map>
#include "StreamSet.h"
#include <string>
#include <vector>
#include <memory>
#include "Logger.h"

namespace sensekit {

    class StreamSetCatalog
    {
    public:
        StreamSetCatalog()
            : m_logger("StreamSetCatalog")
        {}

        ~StreamSetCatalog();

        StreamSetConnection& open_set_connection(std::string uri);
        void close_set_connection(StreamSetConnection* connection);
        StreamSet& get_or_add(std::string uri, bool claim = false);
        StreamSet* find_streamset_for_stream(Stream* stream);

        void visit_sets(std::function<void(StreamSet*)> visitorMethod);
        void destroy_set(StreamSet* set);

    private:
        using StreamSetPtr = std::unique_ptr<StreamSet>;
        using StreamSetMap = std::unordered_map<std::string, StreamSetPtr>;

        StreamSetMap m_streamSets;
        Logger m_logger;
    };
}

#endif /* STREAMSETCATALOG_H */
