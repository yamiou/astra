#ifndef DEVICESTREAMSOURCE_H
#define DEVICESTREAMSOURCE_H

#include "Device.h"
#include "DriverAdapter.h"
#include "StreamSource.h"

namespace sensekit {

    class DeviceStreamSource : public StreamSource
    {
    public:

        DeviceStreamSource(DriverAdapter& adapter,
                           Device& device,
                           sensekit_streamsource_desc_t desc);

        virtual ~DeviceStreamSource() { }

        virtual Stream* create_stream() override;
        virtual void open_stream(Stream* stream) override;
        virtual void destroy_stream(Stream* stream) override;
        virtual void close_stream(Stream* stream) override;

        Device& get_device() { return m_device; };

    protected:

        DriverAdapter& m_adapter;
        Device& m_device;
        stream_handle_t m_deviceStreamHandle;
    };
}

#endif /* DEVICESTREAMSOURCE_H */
