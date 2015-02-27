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

    sensekit_status_t DriverService::open_device(const char *uri, Device** device)
    {
        Device* d = find_device_by_uri(uri);

        if (d)
        {
            d->open();
            *device = d;

            return SENSEKIT_STATUS_SUCCESS;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    Device* DriverService::find_device_by_uri(const char* uri)
    {
        auto it = std::find_if(m_devices.begin(),
                               m_devices.end(),
                               [uri] (Device* d) -> bool
                               {
                                   const char* d_uri = d->get_description().uri;
                                   return strncmp(d_uri, uri, MAX_STRING_FIELD_LENGTH) == 0;
                               });

        if (it == m_devices.end())
        {
            return nullptr;
        }

        return *it;
    }

    Device* DriverService::find_device_by_id(size_t deviceId)
    {
        auto it = std::find_if(m_devices.begin(),
                               m_devices.end(),
                               [deviceId] (Device* d) -> bool
                               {
                                   return d->get_device_id() == deviceId;
                               });

        if (it == m_devices.end())
        {
            return nullptr;
        }

        return *it;
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

    bool DriverService::add_device(DriverAdapter* adapter, const sensekit_device_desc_t& desc)
    {
        Device* device = find_device_by_uri(desc.uri);

        if (device == nullptr)
        {
            device = new Device(*adapter, desc);
            m_devices.push_back(device);
            m_connectedSignal.raise(device);

            return true;
        }

        return false;
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

    void DriverService::adapter_deviceConnected(DriverAdapter* adapter, const sensekit_device_desc_t& desc, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);
        service->add_device(adapter, desc);
    }

    void DriverService::adapter_deviceDisconnected(const sensekit_device_desc_t& desc, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);

        Device* device = service->find_device_by_uri(desc.uri);

        if (device != nullptr)
        {
            service->m_disconnectedSignal.raise(device);
            service->remove_device(device);

            delete device;
        }
    }

    void DriverService::adapter_deviceChanged(const sensekit_device_desc_t& desc, void* context)
    {
        DriverService* service = static_cast<DriverService*>(context);

        Device* device = service->find_device_by_uri(desc.uri);

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
