#include "Device.h"
#include "DeviceStreamSource.h"
#include "ColorStream.h"

namespace sensekit {

    Device::StreamDescriptionList Device::get_available_streams()
    {

    }

    Stream* Device::open_stream(stream_type_t streamType)
    {
        Stream* stream;

        auto it = m_streams.find(streamType);
        if (it != m_streams.end())
        {
            stream = it->second;
        }
        else
        {
            auto source = new DeviceStreamSource(m_driverAdapter, *this);
            ColorStream* stream = new ColorStream(*source);

            m_streams[streamType] = stream;
        }
    }

    void Device::open()
    {
        m_driverAdapter.open_device(this);
    }

    void Device::close()
    {
        if (!is_open()) return;

        m_driverAdapter.close_device(this);
    }
}
