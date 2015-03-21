#include <SenseKit.h>
#include "Stream.h"
#include "StreamBin.h"

#include <cstdint>
#include <algorithm>
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    StreamConnection* Stream::open()
    {
        std::unique_ptr<StreamConnection> conn(new StreamConnection(this));
        conn->set_bin(m_bin);

        StreamConnection* rawPtr = conn.get();

        m_connections.push_back(std::move(conn));

        return rawPtr;
    }

    void Stream::close(StreamConnection* connection)
    {
        auto it = std::find_if(m_connections.cbegin(),
                               m_connections.cend(),
                               [connection] (const std::unique_ptr<StreamConnection>& sc)
                               -> bool
                               {
                                   return sc.get() == connection;
                               });

        if (it != m_connections.cend())
            m_connections.erase(it);
    }

    StreamBin* Stream::create_bin(size_t bufferLengthInBytes)
    {
        int newBinId = m_nextBinId++;

        m_bin = new StreamBin(newBinId, bufferLengthInBytes);
        return m_bin;
    }

    void Stream::destroy_bin(StreamBin* bin)
    {
        delete bin;
    }

    StreamBin* Stream::get_bin_by_id(StreamBinId id)
    {
        return m_bin;
    }

    Stream::~Stream()
    {
        if (m_bin)
            delete m_bin;
    }
}
