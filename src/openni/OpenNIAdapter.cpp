#include "OpenNIAdapter.h"

namespace sensekit {

    sensekit_status_t OpenNIAdapter::initialize(
        device_connected_callback_t connectedCallback,
        device_disconnected_callback_t disconnectedCallback,
        device_changed_callback_t changedCallback,
        void* callbackContext)
    {
        DriverAdapter::initialize(connectedCallback, disconnectedCallback, changedCallback, callbackContext);

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

        const openni::DeviceInfo& info = m_device.getDeviceInfo();

        strncpy(m_desc.uri, info.getUri(), MAX_STRING_FIELD_LENGTH);
        strncpy(m_desc.name, info.getName(), MAX_STRING_FIELD_LENGTH);
        strncpy(m_desc.vendor, info.getVendor(), MAX_STRING_FIELD_LENGTH);
        m_desc.usb_vendor_id = info.getUsbVendorId();
        m_desc.usb_product_id = info.getUsbProductId();

        m_deviceConnectedCallback(this, m_desc, m_callbackContext);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OpenNIAdapter::terminate()
    {

        if (m_initialized)
        {
            m_deviceDisconnectedCallback(m_desc, m_callbackContext);
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

    device_handle_t OpenNIAdapter::open_device(const char *uri)
    {
        return &m_device;
    }

    driver_status_t OpenNIAdapter::close_device(device_handle_t deviceHandle)
    {
        openni::Device* device = static_cast<openni::Device*>(deviceHandle);
        device->close();

        return DRIVER_STATUS_SUCCESS;
    }
}