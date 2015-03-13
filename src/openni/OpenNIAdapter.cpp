#include "OpenNIAdapter.h"
#include "../Device.h"
#include "../Stream.h"
#include "../DeviceStreamSource.h"
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

    sensekit_stream_mode_desc_t create_stream_mode_desc(const openni::VideoMode& mode)
    {
        sensekit_stream_mode_desc_t modeDesc;
        unsigned bytesPerPixel = 0;
        sensekit_pixel_format_t pixelFormat = (sensekit_pixel_format_t)0;

        switch (mode.getPixelFormat())
        {
        case ONI_PIXEL_FORMAT_RGB888:
            pixelFormat = SENSEKIT_PIXEL_FORMAT_RGB888;
            bytesPerPixel = 3;
            break;
        case ONI_PIXEL_FORMAT_DEPTH_1_MM:
            pixelFormat = SENSEKIT_PIXEL_FORMAT_DEPTH_MM;
            bytesPerPixel = 2;
            break;
        case ONI_PIXEL_FORMAT_GRAY8:
            pixelFormat = SENSEKIT_PIXEL_FORMAT_GRAY8;
            bytesPerPixel = 1;
            break;
        case ONI_PIXEL_FORMAT_GRAY16:
            pixelFormat = SENSEKIT_PIXEL_FORMAT_GRAY16;
            bytesPerPixel = 2;
            break;
        }

        modeDesc.pixelFormat = pixelFormat;
        sensekit_frame_desc_t& desc = modeDesc.frameDescription;

        modeDesc.framesPerSecond = mode.getFps();
        desc.height = mode.getResolutionY();
        desc.width = mode.getResolutionX();
        desc.bytesPerPixel = bytesPerPixel;
        desc.length = desc.width * desc.height;

        return modeDesc;
    }

    sensekit_streamsource_desc_t create_streamsource_desc(const openni::SensorInfo& info)
    {
        const openni::Array<openni::VideoMode>& niModes = info.getSupportedVideoModes();

        std::vector<sensekit_stream_mode_desc_t> modes;

        size_t count = niModes.getSize();
        for (size_t i = 0; i < count; i++)
        {
            const openni::VideoMode& niMode = niModes[i];
            sensekit_stream_mode_desc_t d = create_stream_mode_desc(niMode);

            if (d.frameDescription.bytesPerPixel > 0)
            {
                modes.push_back(d);
            }
            else
            {
                cout << "adapter: unsupported mode" << endl;
            }
        }

        count = modes.size();

        sensekit_streamsource_desc_t streamDesc;

        if (count > 0)
        {
            sensekit_stream_mode_desc_t* modeArray =
                new sensekit_stream_mode_desc_t[count];

            std::copy(modes.begin(), modes.end(), modeArray);
            streamDesc.modes = modeArray;
            streamDesc.modeCount = count;
        }
        else
        {
            streamDesc.modes = nullptr;
            streamDesc.modeCount = 0;
        }

        return streamDesc;
    }

    openni::Device* unwrap_device(Device& device)
    {
        return static_cast<openni::Device*>(device.get_handle());
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
        openni::Device* niDevice = unwrap_device(*device);

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

    StreamDescList OpenNIAdapter::get_device_streams(Device* device)
                                  {
        openni::Device* niDevice = unwrap_device(*device);

        const openni::SensorInfo* colorInfo =
            niDevice->getSensorInfo(openni::SensorType::SENSOR_COLOR);

        const openni::SensorInfo* depthInfo =
            niDevice->getSensorInfo(openni::SensorType::SENSOR_DEPTH);

        StreamDescList streamList;

        if (colorInfo)
        {
            sensekit_streamsource_desc_t desc =
                create_streamsource_desc(*colorInfo);
            desc.type = SENSEKIT_STREAM_COLOR;

            streamList.push_back(desc);
        }

        if (depthInfo)
        {
            sensekit_streamsource_desc_t desc =
                create_streamsource_desc(*depthInfo);
            desc.type = SENSEKIT_STREAM_DEPTH;

            streamList.push_back(desc);
        }

        for(auto& ms : streamList)
        {
            for(size_t i = 0; i < ms.modeCount; i++)
            {
                auto& d = ms.modes[i];
                cout << "adapter: supported mode: "
                     << d.frameDescription.width
                     << "x"
                     << d.frameDescription.height
                     << "x"
                     << d.frameDescription.bytesPerPixel
                     << " (" << d.framesPerSecond << " fps)"
                     <<  endl;
            }
        }

        return streamList;
    }


    stream_handle_t OpenNIAdapter::open_stream(DeviceStreamSource* source)
    {
        Device& device = source->get_device();
        openni::Device* niDevice = unwrap_device(device);

        openni::VideoStream* stream = new openni::VideoStream();

        const sensekit_streamsource_desc_t& desc = source->get_description();
        if (desc.type == SENSEKIT_STREAM_COLOR)
        {
            cout << "adapter: opening color stream." << endl;
            stream->create(*niDevice, openni::SensorType::SENSOR_COLOR);
        }
        else
        {
            cout << "adapter: opening depth stream." << endl;
            stream->create(*niDevice, openni::SensorType::SENSOR_DEPTH);
        }

        return stream_handle_t(stream);
    }

    void OpenNIAdapter::close_stream(Stream* stream)
    {

    }

}