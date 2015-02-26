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
        if (NULL == driver)
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

    void DeviceManager::on_device_connected(Device* device)
    {
        cout << "device connected." << endl;
        m_devices.push_back(device);
    }

    void DeviceManager::on_device_disconnected(Device* device)
    {
        cout << "device disconnected." << endl;
    }

    void DeviceManager::on_device_changed(Device* device)
    {
        cout << "device changed." << endl;
    }

    sensekit_status_t DeviceManager::query_for_device(char* uri, Device** device)
    {

        *device = m_devices[0];
        return SENSEKIT_STATUS_SUCCESS;
    }

}
