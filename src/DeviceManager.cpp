#include "DeviceManager.h"
#include "openni/OpenNIAdapter.h"

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
        service->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::query_for_device(char* uri, Device** device)
    {

        return SENSEKIT_STATUS_SUCCESS;
    }

}
