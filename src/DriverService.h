#ifndef DRIVERSERVICE_H
#define DRIVERSERVICE_H

#include <SenseKit.h>

namespace sensekit {

    class Device;

    class DriverService
    {
    public:
        DriverService() {}
        virtual ~DriverService() {}

        virtual sensekit_status_t initialize();
        virtual sensekit_status_t destroy();
        virtual sensekit_status_t has_device_for_uri(char *uri, bool& deviceAvailable);
        virtual sensekit_status_t query_for_device(char* uri, Device** device);
    };
}

#endif /* DRIVERSERVICE_H */
