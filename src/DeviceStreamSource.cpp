#include "DeviceStreamSource.h"

namespace sensekit {

    DeviceStreamSource::DeviceStreamSource(DriverAdapter& adapter,
                                           Device& device)
        : m_adapter(adapter),
          m_device(device)
    { }

    Stream* DeviceStreamSource::create_stream()
    {

    }

    void DeviceStreamSource::destroy_stream(Stream* stream)
    {

    }

}
