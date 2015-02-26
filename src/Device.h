#ifndef DEVICE_H
#define DEVICE_H

#include "DriverAdapter.h"
#include "Stream.h"

namespace sensekit {

    class Device
    {
    public:

        typedef size_t DeviceId;

        Device(DriverAdapter& adapter, device_handle_t deviceHandle)
            : m_driverAdapter(adapter), m_deviceHandle(deviceHandle)
            {}

        virtual ~Device() {}

        Stream* create_stream();

        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        DeviceId get_device_id() { return DeviceId(m_deviceHandle); }

    private:

        DriverAdapter& m_driverAdapter;
        device_handle_t m_deviceHandle;
    };

}


#endif /* DEVICE_H */
