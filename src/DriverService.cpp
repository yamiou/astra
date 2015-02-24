#include "DriverService.h"

namespace sensekit {

    sensekit_status_t DriverService::initialize()
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DriverService::destroy()
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DriverService::has_device_for_uri(char *uri, bool &deviceAvailable)
    {
        deviceAvailable = true;
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DriverService::query_for_device(char* uri, Device** device)
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

}
