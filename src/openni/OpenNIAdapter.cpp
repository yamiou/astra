#include "OpenNIAdapter.hpp"
#include "../SenseKit-adapter-private.h"

namespace sensekit {

    sensekit_status_t OpenNIAdapter::Open()
    {
        openni::Status rc = openni::STATUS_OK;

        const char* deviceURI = openni::ANY_DEVICE;

        rc = openni::OpenNI::initialize();
        rc = m_device.open(deviceURI);

        if (rc != openni::STATUS_OK)
        {
                openni::OpenNI::shutdown();
                return SENSEKIT_STATUS_DEVICE_ERROR;
        }

        rc = m_depthStream.create(m_device, openni::SENSOR_DEPTH);
        if (rc == openni::STATUS_OK)
        {
                rc = m_depthStream.start();
                if (rc != openni::STATUS_OK)
                {
                        m_depthStream.destroy();
                }
        }
        else
        {
                return SENSEKIT_STATUS_DEVICE_ERROR;
        }

        rc = m_colorStream.create(m_device, openni::SENSOR_COLOR);
        if (rc == openni::STATUS_OK)
        {
                rc = m_colorStream.start();
                if (rc != openni::STATUS_OK)
                {
                        m_colorStream.destroy();
                }
        }
        else
        {
                return SENSEKIT_STATUS_DEVICE_ERROR;
        }

        if (!m_depthStream.isValid() || !m_colorStream.isValid())
        {
                openni::OpenNI::shutdown();
                return SENSEKIT_STATUS_DEVICE_ERROR;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OpenNIAdapter::Close()
    {

    }
}


SENSEKIT_BEGIN_DECLS

sensor_handle_t* _create_openni_adapter()
{
    sensekit::OpenNIAdapter* adapter = new sensekit::OpenNIAdapter();
    adapter->Open();

        return reinterpret_cast<sensor_handle_t*>(adapter);
}

void _destroy_openni_adapter(sensor_handle_t* handle)
{
        if (handle == NULL)
        {
                return;
        }

        sensekit::OpenNIAdapter* adapter = reinterpret_cast<sensekit::OpenNIAdapter*>(handle);
        delete adapter;
}

SENSEKIT_END_DECLS