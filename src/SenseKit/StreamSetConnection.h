#ifndef STREAMSETCONNECTION_H
#define STREAMSETCONNECTION_H

#include <SenseKit/sensekit_types.h>

namespace sensekit {

    class StreamSet;

    class StreamSetConnection
    {
    public:
        StreamSetConnection(StreamSet* streamSet)
            : m_streamSet(streamSet)
        {}

        ~StreamSetConnection() = default;

        StreamSetConnection& operator=(const StreamSetConnection& rhs) = delete;
        StreamSetConnection(const StreamSetConnection& conn) = delete;

        StreamSet* get_streamSet() { return m_streamSet; }

        static StreamSetConnection* get_ptr(sensekit_streamsetconnection_t conn)
        {
            return reinterpret_cast<StreamSetConnection*>(conn);
        }

        sensekit_streamsetconnection_t get_handle()
        {
            return reinterpret_cast<sensekit_streamsetconnection_t>(this);
        }

    private:
        StreamSet* m_streamSet;
    };
}


#endif /* STREAMSETCONNECTION_H */
