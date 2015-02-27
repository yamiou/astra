#ifndef DEVICE_H
#define DEVICE_H

#include "DriverAdapter.h"
#include "Stream.h"

namespace sensekit {

    class Device
    {
    public:

        typedef size_t DeviceId;

        Device(DriverAdapter& adapter, const sensekit_device_desc_t& desc)
            : m_driverAdapter(adapter), m_deviceDesc(desc), m_deviceHandle(nullptr)
            {}

        virtual ~Device() {}

        Stream* create_stream();

        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        const sensekit_device_desc_t& get_description() const { return m_deviceDesc; }
        DeviceId get_device_id() { return DeviceId(m_deviceHandle); }

    private:

        DriverAdapter& m_driverAdapter;
        device_handle_t m_deviceHandle;
        const sensekit_device_desc_t& m_deviceDesc;
    };

}


#endif /* DEVICE_H */
