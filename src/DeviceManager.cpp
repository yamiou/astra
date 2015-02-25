#include "DeviceManager.h"
#include "openni/OpenNIAdapter.h"

namespace sensekit {

    sensekit_status_t DeviceManager::initialize()
    {
        add_driver(new OpenNIAdapter());

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::add_driver(DriverService* driver)
    {
        if (NULL == driver)
        {
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        m_drivers.push_back(driver);
        driver->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::find_driver(char* uri, DriverService** service)
    {
        if (NULL == uri)
        {
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        for(DriverService* candidateService : m_drivers)
        {
            bool deviceAvailable;
            candidateService->has_device_for_uri(uri, deviceAvailable);

            if (deviceAvailable)
            {
                *service = candidateService;
                return SENSEKIT_STATUS_SUCCESS;
            }
        }

        *service = NULL;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DeviceManager::query_for_device(char* uri, Device** device)
    {
        DriverService* service;
        sensekit_status_t status = find_driver(uri, &service);

        if (status != SENSEKIT_STATUS_SUCCESS)
        {
            return status;
        }

        if (NULL == service)
        {
            *device = NULL;
            return SENSEKIT_STATUS_SUCCESS;
        }

        Device* d = NULL;
        service->query_for_device(uri, &d);

        *device = d;

        return SENSEKIT_STATUS_SUCCESS;
    }

}
