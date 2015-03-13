#include "DeviceManager.h"
#include "../Device.h"

namespace sensekit { namespace driver {

    bool DeviceManager::device_exists(Device* device)
    {
        if (device == nullptr)
            return false;

        // NOTE: pointer comparison
        auto it = find(m_devices.begin(), m_devices.end(), device);

        if (it != m_devices.end())
            return true;

        return false;
    }

    void DeviceManager::add_device(Device* device)
    {
        if (device == nullptr)
            return;

        if (!device_exists(device))
        {
            cout << "adapter: adding device: " << device->get_description().uri << endl;
            m_devices.push_back(device);
        }
    }

    bool DeviceManager::remove_device(Device* device)
    {
        if (device == nullptr)
            return false;

        // NOTE: pointer comparison
        auto it = find(m_devices.begin(), m_devices.end(), device);

        if (it != m_devices.end())
        {
            m_devices.erase(it);
            return true;
        }

        return false;
    }

    Device* DeviceManager::query_for_device(const char* uri)
    {
        Device* device = nullptr;

        if (find_device_by_uri(uri, &device))
        {
            return device;
        }

        return nullptr;
    }

    bool DeviceManager::find_device_by_uri(const char* uri, Device** device)
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
            *device = nullptr;
            return false;
        }

        *device = *it;

        return true;
    }

    bool DeviceManager::find_device_by_id(size_t deviceId, Device** device)
    {
        auto it = std::find_if(m_devices.begin(),
                               m_devices.end(),
                               [deviceId] (Device* d) -> bool
                               {
                                   return d->get_device_id() == deviceId;
                               });

        if (it == m_devices.end())
        {
            *device = nullptr;
            return false;
        }

        *device = *it;

        return true;
    }

}}
