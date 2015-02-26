#include "DriverService.h"
#include "Signal.h"

#include <algorithm>

namespace sensekit {

    sensekit_status_t DriverService::initialize()
    {
        m_driverAdapter.initialize(
            DriverService::adapter_deviceConnected,
            DriverService::adapter_deviceDisconnected,
            DriverService::adapter_deviceChanged,
            this);

        return SENSEKIT_STATUS_SUCCESS;
    }

    Device* DriverService::find_device(size_t deviceId)
    {
        auto it = std::find_if(m_devices.begin(),
                               m_devices.end(),
                               [deviceId] (Device* d) -> bool
                               {
                                   return d->get_device_id() == deviceId;
                               });

        if (it != m_devices.end())
        {
            return *it;
        }
        else
        {
            return nullptr;
        }

    }

    bool DriverService::device_exists(const Device& device)
    {
        auto it = std::find_if(m_devices.begin(),
                               m_devices.end(),
                               [device] (Device* d) -> bool
                               {
                                   return *d == device;
                               });

        return it != m_devices.end();
    }

    bool DriverService::remove_device(const Device* device)
    {
        auto it = std::find(m_devices.begin(), m_devices.end(), device);

        if (it != m_devices.end())
        {
            m_devices.erase(it);
            return true;
        }

        return false;
    }

    void DriverService::adapter_deviceConnected(DriverAdapter* adapter, device_handle_t deviceHandle, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);
        Device* device = new Device(*adapter, deviceHandle);

        if (!service->device_exists(*device))
        {
            service->m_devices.push_back(device);
            service->m_connectedSignal.raise(device);
        }
        else
        {
            delete device;
        }
    }

    void DriverService::adapter_deviceDisconnected(device_handle_t deviceHandle, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);

        Device* device = service->find_device(Device::DeviceId(deviceHandle));

        if (device != nullptr)
        {
            service->m_disconnectedSignal.raise(device);
            service->remove_device(device);

            delete device;
        }
    }

    void DriverService::adapter_deviceChanged(device_handle_t deviceHandle, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);

        Device* device = service->find_device(Device::DeviceId(deviceHandle));

        if (device != nullptr)
        {
            service->m_changedSignal.raise(device);
        }
    }

    sensekit_status_t DriverService::terminate()
    {
        m_driverAdapter.terminate();

        return SENSEKIT_STATUS_SUCCESS;
    }
}
