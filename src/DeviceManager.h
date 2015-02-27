#ifndef DEVICEMANAGER_H
#define DEVICEMANAGER_H

#include <SenseKit.h>
#include <vector>
#include "Device.h"
#include "DriverAdapter.h"
#include "DriverService.h"

namespace sensekit {

    class DeviceManager
    {

    public:

        DeviceManager() {}
        virtual ~DeviceManager() {}

        sensekit_status_t initialize();
        sensekit_status_t query_for_device(const char* uri, Device** device);
        sensekit_status_t open_device(const char* uri, Device** device);
        sensekit_status_t close_device(Device** device);

    private:

        typedef std::vector<DriverService*> DriverList;

        DriverList m_drivers;

        // TODO: this function is a placeholder for a DriverService factory
        sensekit_status_t add_driver(DriverAdapter* driver);

        void on_device_connected(Device* device);
        void on_device_disconnected(Device* device);
        void on_device_changed(Device* device);
    };

}


#endif /* DEVICEMANAGER_H */
