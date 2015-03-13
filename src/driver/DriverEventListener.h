#ifndef DRIVEREVENTLISTENER_H
#define DRIVEREVENTLISTENER_H

#include "../Device.h"

namespace sensekit { namespace driver {

        class DriverEventListener
        {
        public:
            DriverEventListener() {};
            virtual ~DriverEventListener() {};

            virtual void on_driver_initialized() {}
            virtual void on_driver_terminated() {}
            virtual void on_device_connected(Device* device) = 0;
            virtual void on_device_disconnected(Device* device) = 0;
            virtual void on_device_changed(Device* device) = 0;
        };
}}

#endif /* DRIVEREVENTLISTENER_H */
