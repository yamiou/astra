#include "Context.h"
#include <cstdlib>
#include "StreamSource.h"
#include "Device.h"
#include <iostream>
#include "openni/OniStreamServer.h"

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t Context::initialize()
    {
        if (m_initialized)
            return SENSEKIT_STATUS_SUCCESS;

        m_serverRegistry.register_stream_server(new openni::OniStreamServer());

        m_serverRegistry.initialize();
        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::terminate()
    {
        if (!m_initialized)
            return SENSEKIT_STATUS_SUCCESS;

        m_serverRegistry.terminate();
        m_initialized = false;

        return SENSEKIT_STATUS_SUCCESS;
    }

    void Context::ensure_initialized()
    {
        if (!m_initialized)
        {
            initialize();
        }
    }

    sensekit_status_t Context::open_sensor(const char* uri, sensekit_sensor_t** sensor)
    {
        if (uri == nullptr)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

        ensure_initialized();

        cout << "api: opening device: " << uri << endl;

        // Device* device = m_serverRegistry.query_for_device(uri);

        // if (device != nullptr)
        // {
        //     device->open();
        //     auto streams = device->get_streams();
        //     //Stream* stream = srcs[0]->create_stream();
        //     //stream->open();

        // }

        // *sensor  = new sensekit_sensor_t;
        // (*sensor)->p_deviceHandle = device;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::close_sensor(sensekit_sensor_t** sensor)
    {
        ensure_initialized();

        Device* device = (*sensor)->p_deviceHandle;

        delete *sensor;
        *sensor = nullptr;

        cout << "api: close sensor: " << device->get_description().uri << endl;

        if (device != nullptr)
            device->close();

        return SENSEKIT_STATUS_SUCCESS;
    }
}
