#include "DriverService.h"
#include "Signal.h"

#include <algorithm>
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {


    sensekit_status_t DriverService::initialize()
    {

        //TODO check perf of bind vs lambda

        m_driverAdapter.connectedSignal() +=
            std::bind(&DriverService::adapter_deviceConnected, this, std::placeholders::_1);

        m_driverAdapter.disconnectedSignal() +=
            std::bind(&DriverService::adapter_deviceDisconnected, this, std::placeholders::_1);

        m_driverAdapter.changedSignal() +=
            std::bind(&DriverService::adapter_deviceChanged, this, std::placeholders::_1);

        m_driverAdapter.initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t DriverService::query_for_device(const char* uri, Device** device)
    {
        *device = NULL;
        Device* d = find_device_by_uri(uri);

        if (d)
        {
            *device = d;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    Device* DriverService::find_device_by_uri(const char* uri)
    {
        const DeviceList& devices = m_driverAdapter.get_devices();

        auto it = std::find_if(devices.begin(),
                               devices.end(),
                               [uri] (Device* d) -> bool
                               {
                                   const char* d_uri = d->get_description().uri;
                                   return strncmp(d_uri, uri, MAX_STRING_FIELD_LENGTH) == 0;
                               });

        if (it == devices.end())
        {
            return nullptr;
        }

        return *it;
    }

    Device* DriverService::find_device_by_id(size_t deviceId)
    {
        const DeviceList& devices = m_driverAdapter.get_devices();

        auto it = std::find_if(devices.begin(),
                               devices.end(),
                               [deviceId] (Device* d) -> bool
                               {
                                   return d->get_device_id() == deviceId;
                               });

        if (it == devices.end())
        {
            return nullptr;
        }

        return *it;
    }

    bool DriverService::device_exists(const Device& device)
    {
        const DeviceList& devices = m_driverAdapter.get_devices();

        auto it = std::find_if(devices.begin(),
                               devices.end(),
                               [&device] (Device* d) -> bool
                               {
                                   return *d == device;
                               });

        return it != devices.end();
    }

    void DriverService::adapter_deviceConnected(DeviceConnectedEventArgs args)
    {
        m_connectedSignal.raise(args.device);
    }

    void DriverService::adapter_deviceDisconnected(DeviceDisconnectedEventArgs args)
    {
        Device* device = find_device_by_uri(args.device->get_description().uri);

        if (device != nullptr)
        {
            m_disconnectedSignal.raise(device);
        }
    }

    void DriverService::adapter_deviceChanged(DeviceChangedEventArgs args)
    {
        Device* device = find_device_by_uri(args.device->get_description().uri);

        if (device != nullptr)
        {
            m_changedSignal.raise(device);
        }
    }

    sensekit_status_t DriverService::terminate()
    {
        m_driverAdapter.terminate();

        return SENSEKIT_STATUS_SUCCESS;
    }
}
