#ifndef STREAM_H
#define STREAM_H

#include "Signal.h"
#include "StreamBin.h"
#include <atomic>

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
        ~Stream();

        StreamConnection* open();
        void close(StreamConnection* connection);

        StreamId id() { return m_id; }
        StreamTypeId typeId() { return m_typeId; }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        StreamBin* get_bin_by_id(StreamBinId id);

    private:

        const StreamId m_id{0};
        const StreamTypeId m_typeId{0};

        // make it work, only one bin
        StreamBin* m_bin{nullptr};
        std::atomic_int m_nextBinId{0};

        Signal<StreamConnection*> m_connectionAddedSignal;
        Signal<StreamConnection*> m_connectionRemovedSignal;
        Signal<void> m_getParameterSignal;
        Signal<void> m_setParameterSignal;
    };
}

#endif /* STREAM_H */
