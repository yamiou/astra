#ifndef ASTRA_SENSOR_LISTENER_H
#define ASTRA_SENSOR_LISTENER_H

#include "astra_sensor.hpp"

namespace astra { namespace devices {

    class sensor_listener
    {
    public:
        virtual void state_changed(sensor::shared_ptr sensor) {}
        virtual void property_changed(sensor::shared_ptr sensor, sensor::property_id id) {}
    };
}}

#endif /* ASTRA_SENSOR_LISTENER_H */
