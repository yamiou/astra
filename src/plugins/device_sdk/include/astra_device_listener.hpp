#ifndef ASTRA_DEVICE_LISTENER_H
#define ASTRA_DEVICE_LISTENER_H

#include "astra_device.hpp"

namespace astra { namespace devices {

    class device_listener
    {
    public:
        virtual ~device_listener() {}
        virtual void state_changed(device::shared_ptr device) = 0;
        virtual void property_changed(device::shared_ptr device, device::property_id id) = 0;
        virtual void connectivity_changed(device::shared_ptr device) = 0;
    };
}}

#endif /* ASTRA_DEVICE_LISTENER_H */
