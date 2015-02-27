#ifndef CONTEXT_H
#define CONTEXT_H

#include <SenseKit.h>
#include <vector>
#include "Sensor.h"
#include "SensorAdapter.h"
#include "DeviceManager.h"

struct _sensekit_sensor
{
    sensekit::Device* p_deviceHandle;
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

        sensekit_status_t initialize();
        sensekit_status_t open_sensor(const char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t close_sensor(sensekit_sensor_t** sensor);

    private:
        vector<Sensor*> m_sensors;
        DeviceManager m_deviceManager;
        bool m_initialized;

        SensorAdapter* get_sensor_adapter();
        void ensure_initialized();
    };
}

#endif /* CONTEXT_H */
