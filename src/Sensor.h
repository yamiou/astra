#ifndef SENSOR_H
#define SENSOR_H

#include <SenseKit.h>
#include "SensorAdapter.h"

namespace sensekit {

    class Sensor
    {
    public:
        Sensor(SensorAdapter* adapter) :
            m_adapter(adapter)
            {}

        virtual ~Sensor() { }

        sensekit_status_t Open(void);
        sensekit_status_t Close(void);
    private:
        SensorAdapter* m_adapter;
    };

}


#endif /* SENSOR_H */
