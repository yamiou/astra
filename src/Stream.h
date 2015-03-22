#ifndef STREAM_H
#define STREAM_H

#include "Signal.h"
#include "StreamBin.h"
#include <atomic>
#include <memory>
#include <vector>
#include <map>
#include "StreamConnection.h"

namespace sensekit {

    class StreamBin;

    using StreamId = unsigned;
    using StreamTypeId = unsigned;

    class Stream
    {
    public:
        Stream(StreamId id, StreamTypeId typeId, int index)
            : m_id(id),
              m_typeId(typeId)
        {
            m_nextBinId = 0;
        }

        ~Stream();

        StreamConnection* open();
        void close(StreamConnection* connection);

        StreamId get_id() { return m_id; }
        StreamTypeId get_typeId() { return m_typeId; }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        StreamBin* get_bin_by_id(StreamBinId id);

    private:
        using ConnPtr = std::unique_ptr<StreamConnection>;
        using ConnectionList = std::vector<ConnPtr>;

        using BinPtr = std::unique_ptr<StreamBin>;
        using BinMap = std::map<StreamBinId, BinPtr>;

        const StreamId m_id{0};
        const StreamTypeId m_typeId{0};

        ConnectionList m_connections;
        BinMap m_bins;

        std::atomic_int m_nextBinId;

        Signal<StreamConnection*> m_connectionAddedSignal;
        Signal<StreamConnection*> m_connectionRemovedSignal;
        Signal<void> m_getParameterSignal;
        Signal<void> m_setParameterSignal;
    };
}

#endif /* STREAM_H */
