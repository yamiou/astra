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
        return new StreamConnection(this);
    }

    void Stream::close(StreamConnection* connection)
    {
        delete connection;
    }

    StreamBin* Stream::create_bin(size_t bufferLengthInBytes)
    {
        cout << "creating bin." << endl;

        return new StreamBin(bufferLengthInBytes);
    }

    void Stream::destroy_bin(StreamBin* bin)
    {
        delete bin;
    }
}
