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
        service->registerDeviceConnectedCallback([this] (Device* d) { this->m_devices.push_back(d);});
        service->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::query_for_device(char* uri, Device** device)
    {

        *device = m_devices[0];
        return SENSEKIT_STATUS_SUCCESS;
    }

}
