#ifndef DEVICE_H
#define DEVICE_H

#include "DriverService.h"
#include "Stream.h"

namespace sensekit {

    class Device
    {
    public:
        Device(DriverService& driverService)
            : m_driverService(driverService)
            {}

        virtual ~Device() {}

        Stream* create_stream();

    private:

        DriverService& m_driverService;
    };

}


#endif /* DEVICE_H */
