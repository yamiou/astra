#include "Device.h"

namespace sensekit {

    StreamList& Device::get_streams()
    {
        if (!m_streams)
        {
            m_streams = new StreamList();
            auto descs =
                m_driverAdapter.get_device_streams(this);
            // TODO: Populate stream m_streams
        }

        return *m_streams;
    }

    Device::~Device()
    {
        delete m_deviceDesc;

        if (m_streams)
        {
            auto it = m_streams->begin();
            while (it != m_streams->end())
            {
                Stream* stream = *it;
                it = m_streams->erase(it);

                delete stream;
            }
            delete m_streams;
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
