#include <SenseKit.h>
#include "Stream.h"
#include "StreamConnection.h"
#include "StreamBin.h"
#include <cstdint>

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit {

    StreamConnection* Stream::open()
    {
        auto connection = new StreamConnection(this);
        connection->set_bin(m_bin);

        return connection;
    }

    void Stream::close(StreamConnection* connection)
    {
        delete connection;
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
