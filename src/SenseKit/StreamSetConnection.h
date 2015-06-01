#ifndef STREAMSETCONNECTION_H
#define STREAMSETCONNECTION_H

#include <SenseKit/sensekit_types.h>
#include "StreamReader.h"
#include <vector>
#include <memory>

namespace sensekit {

    class StreamSet;

    class StreamSetConnection
    {
    public:
        StreamSetConnection(StreamSet* streamSet)
            : m_streamSet(streamSet)
        {}

        StreamSetConnection() {}

        ~StreamSetConnection() = default;

        StreamSetConnection& operator=(const StreamSetConnection& rhs) = delete;
        StreamSetConnection(const StreamSetConnection& conn) = delete;

        StreamSet* get_streamSet() { return m_streamSet; }

        void connect_to(StreamSet* streamSet)
        {
            m_streamSet = streamSet;
        }

        StreamReader* create_reader() { return nullptr; };

        bool is_connected() { return m_streamSet != nullptr; }

        static StreamSetConnection* get_ptr(sensekit_streamsetconnection_t conn)
        {
            return reinterpret_cast<StreamSetConnection*>(conn);
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
    };
}


#endif /* STREAMSETCONNECTION_H */
