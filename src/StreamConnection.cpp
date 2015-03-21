#include "StreamConnection.h"
#include "StreamBin.h"
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    void StreamConnection::set_bin(StreamBin* new_bin)
    {
        if (m_bin)
        {
            m_bin->dec_ref();
        }

        m_bin = new_bin;

        if (m_bin)
        {
            m_bin->add_ref();
        }
    }

    StreamConnection::~StreamConnection()
    {
        set_bin(nullptr);
    }
}
