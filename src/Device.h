#ifndef DEVICE_H
#define DEVICE_H

#include "DriverAdapter.h"
#include "Stream.h"

namespace sensekit {

    class Device
    {
    public:
        Device(DriverAdapter& adapter, device_handle_t deviceHandle)
            : m_driverAdapter(adapter), m_deviceHandle(deviceHandle)
            {}

        virtual ~Device() {}

        Stream* create_stream();

    private:

        DriverAdapter& m_driverAdapter;
        device_handle_t m_deviceHandle;
    };

}


#endif /* DEVICE_H */
