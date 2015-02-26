#include "DriverService.h"
#include <iostream>

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

    void DriverService::adapter_deviceConnected(void *context)
    {
        cout << "device connected" << endl;
    }

    void DriverService::adapter_deviceDisconnected(void *context)
    {

    }




}
