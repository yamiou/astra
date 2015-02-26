#ifndef DRIVERSERVICE_H
#define DRIVERSERVICE_H

#include <SenseKit.h>
#include "DriverAdapter.h"

namespace sensekit {

    class Device;

    class DriverService
    {
    public:
        DriverService(DriverAdapter& driverAdapter)
            : m_driverAdapter(driverAdapter)
            {}

        virtual ~DriverService() {}

        virtual sensekit_status_t initialize();
        virtual sensekit_status_t destroy();


    private:

        DriverAdapter& m_driverAdapter;

        static void adapter_deviceConnected(void* context);
        static void adapter_deviceDisconnected(void* context);
    };
}

#endif /* DRIVERSERVICE_H */
