#include "DriverService.h"
#include <iostream>
#include "Signal.h"

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t DriverService::initialize()
    {
        m_driverAdapter.initialize(
            DriverService::adapter_deviceConnected,
            DriverService::adapter_deviceDisconnected,
            this);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DriverService::destroy()
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    void DriverService::adapter_deviceConnected(DriverAdapter* adapter, device_handle_t deviceHandle, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);
        Device* device = new Device(*adapter, deviceHandle);
        service->m_connectedSignal.raise(device);
    }

    void DriverService::adapter_deviceDisconnected(void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);
        service->m_disconnectedSignal.raise();
    }

    void DriverService::adapter_deviceChanged(void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);
        service->m_changedSignal.raise();
    }




}
