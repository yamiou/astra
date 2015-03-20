#include "StreamBin.h"

namespace sensekit {
    buffer* StreamBin::get_back_buffer()
    {
        return nullptr;
    }

    void StreamBin::swap_bin_buffers(buffer*& old_back_buf, buffer*& new_back_buf)
    {

    }

    buffer* StreamBin::get_front_buffer()
    {
        return nullptr;
    }
}
