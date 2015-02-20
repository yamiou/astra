#include "Context.h"
#include <cstdlib>
#include "openni/OpenNIAdapter.hpp"

namespace sensekit {

    sensekit_status_t Context::Initialize()
    {
        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    void Context::EnsureInitialized()
    {
        if (!m_initialized)
        {
            Initialize();
        }
    }

    sensekit_status_t Context::OpenSensor(char* uri, sensekit_sensor_t** sensor)
    {
        if (NULL == uri)
        {
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        EnsureInitialized();

        Sensor* s = new Sensor(GetSensorAdapter());
        s->Open();

        *sensor  = (sensekit_sensor_t*)malloc(sizeof(sensekit_sensor_t));
        (*sensor)->sensor_ptr = s;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::CloseSensor(sensekit_sensor_t** sensor)
    {
        EnsureInitialized();

        Sensor* s = (*sensor)->sensor_ptr;
        s->Close();

        delete s;

        free(*sensor);

        *sensor = NULL;

        return SENSEKIT_STATUS_SUCCESS;
    }

    SensorAdapter* Context::GetSensorAdapter()
    {
        return new OpenNIAdapter();
    }
}
