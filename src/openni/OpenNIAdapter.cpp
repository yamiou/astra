#include "OpenNIAdapter.h"
#include "../Device.h"
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_device_desc_t* create_device_desc(const openni::DeviceInfo& info)
    {
        sensekit_device_desc_t* desc = new sensekit_device_desc_t;

        strncpy(desc->uri, info.getUri(), MAX_STRING_FIELD_LENGTH);
        strncpy(desc->name, info.getName(), MAX_STRING_FIELD_LENGTH);
        strncpy(desc->vendor, info.getVendor(), MAX_STRING_FIELD_LENGTH);
        desc->usbVendorId = info.getUsbVendorId();
        desc->usbProductId = info.getUsbProductId();

        return desc;
    }

    sensekit_status_t OpenNIAdapter::initialize()
    {

        register_for_events();
        openni::Status rc = openni::STATUS_OK;

        rc = openni::OpenNI::initialize();

        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    void OpenNIAdapter::on_device_connected(const openni::DeviceInfo* info)
    {
        sensekit_device_desc_t* desc = create_device_desc(*info);
        Device* device = new Device(*this, desc);

        cout << "adapter: device connected: " << device->get_description().uri << endl;

        add_device(device);

        device->on_connected();

        raiseConnected(device);
    }

    void OpenNIAdapter::on_device_disconnected(const openni::DeviceInfo* info)
    {
        Device* device = nullptr;

        if (find_device_by_uri(info->getUri(), &device))
        {
            cout << "adapter: device disconnected: " << device->get_description().uri << endl;
            device->on_disconnected();
            raiseDisconnected(device);
        }
    }

    void OpenNIAdapter::on_device_changed(const openni::DeviceInfo* info,
                                          openni::DeviceState state)
    {

    }

    sensekit_status_t OpenNIAdapter::terminate()
    {
        if (m_initialized)
        {
            cout << "adapter: terminating. cleaning up "
                 << m_devices.size()
                 << " devices" << endl;

            auto it = m_devices.begin();

            while (it != m_devices.end())
            {
                Device* device = *it;

                cout << "adapter: killing device " << device->get_description().uri << endl;

                device->on_disconnected();
                raiseDisconnected(device);
                it = m_devices.erase(it);

                destroy_device(device);
            }

            unregister_for_events();

            openni::OpenNI::shutdown();
        }

        m_initialized = false;

        cout << "adapter: openni terminated" << endl;

        return SENSEKIT_STATUS_SUCCESS;
    }

    void OpenNIAdapter::destroy_device(Device* device)
    {
        openni::Device* niDevice = static_cast<openni::Device*>(device->get_handle());
        device->set_handle(nullptr);

        if (niDevice)
        {
            delete niDevice;
        }

        delete device;
    }

    sensekit_status_t OpenNIAdapter::has_device_for_uri(const char* uri, bool &deviceAvailable)
    {
        Device* unused;
        deviceAvailable = find_device_by_uri(uri, &unused);

        return SENSEKIT_STATUS_SUCCESS;
    }

    void OpenNIAdapter::open_device(Device* device)
    {
        openni::Device* niDevice = new openni::Device();
        openni::Status rc = niDevice->open(device->get_description().uri);

        if (rc == openni::STATUS_OK)
        {
            cout << "adapter: openni device opened." << endl;
            device->set_handle(static_cast<device_handle_t>(niDevice));
        }
    }

    driver_status_t OpenNIAdapter::close_device(Device* device)
    {
        cout << "adapter: openni device closing." << endl;

        openni::Device* niDevice = static_cast<openni::Device*>(device->get_handle());

        // TODO: Handle open streams
        niDevice->close();
        delete niDevice;

        device->set_handle(nullptr);

        cout << "adapter: openni device closed" << endl;

        return DRIVER_STATUS_SUCCESS;
    }

    void OpenNIAdapter::register_for_events()
    {
        openni::OpenNI::addDeviceConnectedListener(&m_listener);
        openni::OpenNI::addDeviceDisconnectedListener(&m_listener);
        openni::OpenNI::addDeviceStateChangedListener(&m_listener);
    }

    void OpenNIAdapter::unregister_for_events()
    {
        openni::OpenNI::removeDeviceConnectedListener(&m_listener);
        openni::OpenNI::removeDeviceDisconnectedListener(&m_listener);
        openni::OpenNI::removeDeviceStateChangedListener(&m_listener);
    }

    stream_handle_t OpenNIAdapter::open_stream(device_handle_t deviceHandle, stream_type_t streamType)
    {
        switch (streamType)
        {
        case SENSEKIT_STREAM_COLOR:
            return &m_colorStream;
            break;
        case SENSEKIT_STREAM_DEPTH:
            return &m_depthStream;
            break;
        }

        return nullptr;
    }

    void OpenNIAdapter::close_stream(device_handle_t deviceHandle, stream_handle_t streamHandle)
    {

    }

    driver_status_t OpenNIAdapter::get_available_streams(
        device_handle_t deviceHandle,
        const sensekit_stream_desc_t* descArray,
        size_t* arraySize)
    {

    }

}