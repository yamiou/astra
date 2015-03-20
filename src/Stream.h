#ifndef STREAM_H
#define STREAM_H

#include "Signal.h"

namespace sensekit {
    class StreamBin;
    class StreamConnection;

    using StreamId = unsigned;

    using StreamTypeId = unsigned;

    class Stream
    {
    public:
        Stream(StreamId id, StreamTypeId typeId, int index)
            : m_id(id), m_typeId(typeId)
            {}
        StreamConnection* open();
        void close(StreamConnection* connection);
        StreamId id() { return m_id; }
        StreamTypeId typeId() { return m_typeId; }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

    private:
        const StreamId m_id;
        const StreamTypeId m_typeId;

        Signal<StreamConnection*> m_connectionAddedSignal;
        Signal<StreamConnection*> m_connectionRemovedSignal;
        Signal<void> m_getParameterSignal;
        Signal<void> m_setParameterSignal;
    };
}

#endif /* STREAM_H */
