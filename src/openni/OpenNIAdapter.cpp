#include "OpenNIAdapter.h"
#include "../Device.h"

namespace sensekit {

    sensekit_status_t OpenNIAdapter::initialize()
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

        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OpenNIAdapter::destroy()
    {
        if (m_initialized)
        {
            openni::OpenNI::shutdown();
        }

        m_initialized = false;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OpenNIAdapter::has_device_for_uri(char *uri, bool &deviceAvailable)
    {
        deviceAvailable = true;
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OpenNIAdapter::query_for_device(char *uri, sensekit::Device **device)
    {
        *device = new Device(*this);
        return SENSEKIT_STATUS_SUCCESS;

    }

}