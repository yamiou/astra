#ifndef DEVICEREGISTRY_H
#define DEVICEREGISTRY_H

#include <SenseKit.h>
#include <map>
#include <string>
#include "Device.h"
#include "DriverAdapter.h"

namespace sensekit {

    class DeviceRegistry
    {
    public:

        DeviceRegistry() {}
        ~DeviceRegistry() {}

        Device* query_by_uri(const std::string& uri);
        bool register_device(const std::string& uri, Device* device);
        bool unregister_device(const std::string& uri);

        void clear();

    private:
        using DeviceMap = std::map<std::string, Device*>;

        DeviceMap m_deviceMap;
    };

}

#endif /* DEVICEREGISTRY_H */
