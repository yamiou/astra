#include "DeviceStreamSource.h"

namespace sensekit {

    DeviceStreamSource::DeviceStreamSource(DriverAdapter& adapter,
                                           Device& device,
                                           sensekit_streamsource_desc_t desc)

        : StreamSource(desc),
          m_adapter(adapter),
          m_device(device),
          m_deviceStreamHandle(nullptr)

    { }

    void DeviceStreamSource::open_stream(Stream* stream)
    {
        if (!m_deviceStreamHandle)
        {
            m_deviceStreamHandle = m_adapter.open_stream(this);
        }
    }

    void DeviceStreamSource::close_stream(Stream* stream)
    {

    }

    Stream* DeviceStreamSource::create_stream()
    {
        m_activeStreams++;
        return new Stream(*this);
    }

    void DeviceStreamSource::destroy_stream(Stream* stream)
    {
        m_activeStreams--;
        delete stream;
    }

}
