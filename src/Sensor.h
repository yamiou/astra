#ifndef SENSOR_H
#define SENSOR_H

#include <SenseKit.h>
#include "SensorAdapter.h"

namespace sensekit {

    class Sensor
    {
    public:
        Sensor() { }
        virtual ~Sensor() { }

        sensekit_status_t open(void);
        sensekit_status_t close(void);
    private:

    };

}


#endif /* SENSOR_H */
