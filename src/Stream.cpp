#include "Stream.h"
#include "StreamConnection.h"

namespace sensekit {

    StreamConnection* Stream::open()
    {
        return nullptr;
    }

    void Stream::close(StreamConnection* connection)
    {

    }

    StreamBin* Stream::create_bin(size_t byteLength)
    {
        return nullptr;
    }

    void Stream::destroy_bin(StreamBin* bin)
    {
    }
}
