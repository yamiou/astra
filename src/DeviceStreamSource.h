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
                           Device& device);

        virtual ~DeviceStreamSource() { }

        virtual Stream* create_stream() override;
        virtual void destroy_stream(Stream* stream) override;

    protected:

        DriverAdapter& m_adapter;
        Device& m_device;
        stream_handle_t m_streamHandle;

    };
}


#endif /* DEVICESTREAMSOURCE_H */
