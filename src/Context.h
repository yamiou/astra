#ifndef CONTEXT_H
#define CONTEXT_H

#include <SenseKit.h>
#include <vector>
#include "Sensor.h"
#include "SensorAdapter.h"

struct _sensekit_sensor
{
    sensekit::Sensor* sensor_ptr;
};

using std::vector;

namespace sensekit {

    class Context
    {
    public:
        Context()
            :
            m_initialized(false)
            {};

        virtual ~Context() {};

        sensekit_status_t Initialize();
        sensekit_status_t OpenSensor(char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t CloseSensor(sensekit_sensor_t** sensor);

    private:
        vector<Sensor*> m_sensors;
        bool m_initialized;

        SensorAdapter* GetSensorAdapter();
        void EnsureInitialized();
    };
}

#endif /* CONTEXT_H */
