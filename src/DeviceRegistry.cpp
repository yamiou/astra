#include "DeviceRegistry.h"

#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    Device* DeviceRegistry::query_by_uri(const std::string& uri)
    {
        if (uri.empty() || m_deviceMap.size() == 0)
            return nullptr;

        auto it = m_deviceMap.find(uri);

        if (it == m_deviceMap.end())
            return nullptr;

        return it->second;
    }

    bool DeviceRegistry::register_device(const std::string &uri, sensekit::Device *device)
    {
        if (uri.empty() || device == nullptr)
            return false;

        if (m_deviceMap.find(uri) != m_deviceMap.end())
            return false;

        m_deviceMap.insert(DeviceMap::value_type(uri, device));

        return true;
    }

    bool DeviceRegistry::unregister_device(const std::string &uri)
    {
        if (uri.empty())
            return false;

        auto it = m_deviceMap.find(uri);

        if (it == m_deviceMap.end())
            return false;

        m_deviceMap.erase(it);

        return true;
    }

    void DeviceRegistry::clear()
    {
        m_deviceMap.clear();
    }


}
