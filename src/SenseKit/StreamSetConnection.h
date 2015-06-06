#ifndef STREAMSETCONNECTION_H
#define STREAMSETCONNECTION_H

#include "Registry.h"
#include <SenseKit/sensekit_types.h>
#include "StreamReader.h"
#include <vector>
#include <memory>
#include "Logger.h"

namespace sensekit {

    class StreamSet;

    class StreamSetConnection : public TrackedInstance<StreamSetConnection>
    {
    public:
        StreamSetConnection(StreamSet* streamSet)
            : m_streamSet(streamSet),
              m_logger("StreamSetConnection")
        {}

        ~StreamSetConnection()
        {
            m_logger.trace("destroying StreamSetConnection: %p", this);
        }

        StreamSetConnection& operator=(const StreamSetConnection& rhs) = delete;
        StreamSetConnection(const StreamSetConnection& conn) = delete;

        StreamSet* get_streamSet() { return m_streamSet; }

        StreamReader* create_reader();
        bool destroy_reader(StreamReader* reader);

        bool is_connected() { return m_streamSet != nullptr; }

        static StreamSetConnection* get_ptr(sensekit_streamsetconnection_t conn)
        {
            return Registry::get<StreamSetConnection>(conn);
        }

        sensekit_streamsetconnection_t get_handle()
        {
            return reinterpret_cast<sensekit_streamsetconnection_t>(this);
        }

    private:
        StreamSet* m_streamSet{nullptr};

        using ReaderPtr = std::unique_ptr<StreamReader>;
        using ReaderList = std::vector<ReaderPtr>;

        ReaderList m_readers;

        Logger m_logger;
    };
}


#endif /* STREAMSETCONNECTION_H */
