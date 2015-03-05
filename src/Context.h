#ifndef CONTEXT_H
#define CONTEXT_H

#include <SenseKit.h>
#include "Sensor.h"
#include "SensorAdapter.h"
#include "DeviceManager.h"

struct _sensekit_sensor
{
    sensekit::Device* p_deviceHandle;
};

namespace sensekit {

    class Context
    {
    public:
        Context()
            :
            m_initialized(false)
            { }

         ~Context() { }

        sensekit_status_t initialize();
        sensekit_status_t terminate();
        sensekit_status_t open_sensor(const char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t close_sensor(sensekit_sensor_t** sensor);

    private:
        DeviceManager m_deviceManager;
        bool m_initialized;

        void ensure_initialized();
    };
}

#endif /* CONTEXT_H */
