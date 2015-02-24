#ifndef DEVICEMANAGER_H
#define DEVICEMANAGER_H

#include <SenseKit.h>
#include <vector>
#include "Device.h"
#include "DriverService.h"

namespace sensekit {

    class DeviceManager
    {

    public:
        DeviceManager() {}
        virtual ~DeviceManager() {}

        sensekit_status_t initialize();
        sensekit_status_t query_for_device(char* uri, Device** device);
    private:

        typedef std::vector<DriverService*> DriverList;

        DriverList m_drivers;
        sensekit_status_t add_driver(DriverService* driver);
        sensekit_status_t find_driver(char* uri, DriverService** service);



    };

}


#endif /* DEVICEMANAGER_H */
