#include "DeviceManager.h"
#include "openni/OpenNIAdapter.h"

#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t DeviceManager::initialize()
    {
        add_driver(new OpenNIAdapter());

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::add_driver(DriverAdapter* driver)
    {
        if (driver == nullptr)
        {
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        DriverService* service = new DriverService(*driver);
        m_drivers.push_back(service);

        service->registerDeviceConnectedCallback(
            std::bind(&DeviceManager::on_device_connected, this, std::placeholders::_1));

        service->registerDeviceDisconnectedCallback(
            std::bind(&DeviceManager::on_device_disconnected, this, std::placeholders::_1));

        service->registerDeviceChangedCallback(
            std::bind(&DeviceManager::on_device_changed, this, std::placeholders::_1));

        service->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::open_device(const char *uri, Device **device)
    {
        *device = NULL;
        for(auto* service : m_drivers)
        {
            Device* d;
            service->query_for_device(uri, &d);

            if (d)
            {
                d->open();
                *device = d;
                return SENSEKIT_STATUS_SUCCESS;
            }
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::close_device(Device **device)
    {
        (*device)->close();
        *device = NULL;

        return SENSEKIT_STATUS_SUCCESS;
    }

    void DeviceManager::on_device_connected(Device* device)
    {
        cout << "device connected: "
             << device->get_description().vendor
             << " "
             << device->get_description().name
             << endl;
    }

    void DeviceManager::on_device_disconnected(Device* device)
    {
        cout << "device disconnected." << endl;
    }

    void DeviceManager::on_device_changed(Device* device)
    {
        cout << "device changed." << endl;
    }

    sensekit_status_t DeviceManager::query_for_device(const char* uri, Device** device)
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

}
