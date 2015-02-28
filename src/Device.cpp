#include "Device.h"

namespace sensekit {

    Stream* Device::open_stream()
    {
        return nullptr;
    }

    void Device::open()
    {
        m_deviceHandle = m_driverAdapter.open_device(m_deviceDesc.uri);
    }

    void Device::close()
    {
        if (!is_open()) return;
        m_driverAdapter.close_device(m_deviceHandle);
    }
}
