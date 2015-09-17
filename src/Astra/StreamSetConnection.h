#ifndef STREAMSETCONNECTION_H
#define STREAMSETCONNECTION_H

#include "Registry.h"
#include <Astra/astra_types.h>
#include "StreamReader.h"
#include <vector>
#include <memory>
#include "Logger.h"

namespace astra {

    class StreamSet;

    class StreamSetConnection : public TrackedInstance<StreamSetConnection>
    {
    public:
        StreamSetConnection(StreamSet* streamSet)
            : m_streamSet(streamSet)
        {}

        ~StreamSetConnection()
        {
            LOG_TRACE("StreamSetConnection", "destroying StreamSetConnection: %p", this);
        }

        StreamSetConnection& operator=(const StreamSetConnection& rhs) = delete;
        StreamSetConnection(const StreamSetConnection& conn) = delete;

        StreamSet* get_streamSet() { return m_streamSet; }

        StreamReader* create_reader();
        bool destroy_reader(StreamReader* reader);

        bool is_connected() { return m_streamSet != nullptr; }

        static StreamSetConnection* get_ptr(astra_streamsetconnection_t conn)
        {
            return Registry::get<StreamSetConnection>(conn);
        }

        astra_streamsetconnection_t get_handle()
        {
            return reinterpret_cast<astra_streamsetconnection_t>(this);
        }

    private:
        StreamSet* m_streamSet{nullptr};

        using ReaderPtr = std::unique_ptr<StreamReader>;
        using ReaderList = std::vector<ReaderPtr>;

        ReaderList m_readers;
    };
}


#endif /* STREAMSETCONNECTION_H */
