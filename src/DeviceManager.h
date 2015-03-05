#ifndef DEVICEMANAGER_H
#define DEVICEMANAGER_H

#include <SenseKit.h>
#include <vector>
#include <array>
#include <map>
#include "Device.h"
#include "DriverAdapter.h"

namespace sensekit {


    class DeviceManager
    {

    public:

        DeviceManager() {}
        ~DeviceManager() {}

        void initialize();
        void terminate();

        Device* query_for_device(const char* uri);

    private:

        const static unsigned DRIVER_EVENT_COUNT = 3;
        using CallbackIdList = std::array<size_t, DRIVER_EVENT_COUNT>;

        struct DriverEntry
        {
            DriverAdapter* adapter;
            CallbackIdList callbackList;
        };

        using DriverList = std::vector<DriverEntry>;

        DriverList m_adapters;

        // TODO: this function is a placeholder for a adapter discovery/factory
        sensekit_status_t add_driver(DriverAdapter* driver);

        CallbackIdList register_for_device_events(DriverAdapter& driver);
        void unregister_for_device_events(DriverAdapter& adapter, const CallbackIdList& callbackList);

        void on_device_connected(Device* device);
        void on_device_disconnected(Device* device);
        void on_device_changed(Device* device);
    };

}

#endif /* DEVICEMANAGER_H */
