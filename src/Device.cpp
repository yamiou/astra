#include "Device.h"
#include "DeviceStreamSource.h"
#include "ColorStream.h"

namespace sensekit {

    Stream* Device::open_stream(sensekit_streamtype streamType)
    {
        // Stream* stream;

        // auto it = m_streams.find(streamType);
        // if (it != m_streams.end())
        // {
        //     stream = it->second;
        // }
        // else
        // {
        //     auto source = new DeviceStreamSource(m_driverAdapter, *this);
        //     ColorStream* stream = new ColorStream(*source);

        //     m_streams[streamType] = stream;
        // }
    }

    StreamSourceList& Device::get_stream_sources()
    {
        if (!m_streamSources)
        {
            m_streamSources = new StreamSourceList();
            auto descs =
                m_driverAdapter.get_device_stream_sources(this);

            for(auto& desc : descs)
            {
                StreamSource* streamSource =
                    new DeviceStreamSource(m_driverAdapter, *this, desc);
                m_streamSources->push_back(streamSource);
            }
        }

        return *m_streamSources;
    }

    Device::~Device()
    {
        delete m_deviceDesc;

        if (m_streamSources)
        {
            auto it = m_streamSources->begin();
            while (it != m_streamSources->end())
            {
                StreamSource* source = *it;
                it = m_streamSources->erase(it);

                delete source;
            }
            delete m_streamSources;
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
