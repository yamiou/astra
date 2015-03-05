#include "DeviceManager.h"
#include "openni/OpenNIAdapter.h"

#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    void DeviceManager::initialize()
    {
        add_driver(new OpenNIAdapter());
    }

    void DeviceManager::terminate()
    {
        for(auto& entry : m_adapters)
        {
            DriverAdapter* pAdapter = entry.adapter;
            pAdapter->terminate();

            unregister_for_device_events(*pAdapter, entry.callbackList);
        }
    }

    sensekit_status_t DeviceManager::add_driver(DriverAdapter* driver)
    {
        if (driver == nullptr)
        {
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        CallbackIdList idList = register_for_device_events(*driver);

        DriverEntry entry;
        entry.adapter = driver;
        entry.callbackList = idList;

        m_adapters.push_back(entry);

        driver->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    DeviceManager::CallbackIdList DeviceManager::register_for_device_events(DriverAdapter& driver)
    {
        DeviceManager::CallbackIdList idList;

        idList[0] = driver.connectedSignal() +=
            [this] (DeviceConnectedEventArgs args) { on_device_connected(args.device); };

        idList[1] = driver.disconnectedSignal() +=
            [this] (DeviceDisconnectedEventArgs args) { on_device_disconnected(args.device); };

        idList[2] = driver.changedSignal() +=
            [this] (DeviceChangedEventArgs args) { on_device_changed(args.device); };

        return idList;
    }

    void DeviceManager::unregister_for_device_events(DriverAdapter& adapter, const CallbackIdList& callbackList)
    {
        adapter.connectedSignal() -= callbackList[0];
        adapter.disconnectedSignal() -= callbackList[1];
        adapter.changedSignal() -= callbackList[2];
    }

    void DeviceManager::on_device_connected(Device* device)
    {
        cout << "deviceManager: device connected: "
             << device->get_description().uri
             << ": "
             << device->get_description().vendor
             << " "
             << device->get_description().name
             << endl;
    }

    void DeviceManager::on_device_disconnected(Device* device)
    {
        cout << "deviceManager: device disconnected: " << device->get_description().uri << endl;
    }

    void DeviceManager::on_device_changed(Device* device)
    {
        cout << "deviceManager: device changed." << endl;
    }

    Device* DeviceManager::query_for_device(const char* uri)
    {
        cout << "deviceManager: searching for " << uri;

        for(auto& entry : m_adapters)
        {
            DriverAdapter* pAdapter = entry.adapter;

            Device* pDevice = nullptr;
            pDevice = pAdapter->query_for_device(uri);

            if (pDevice)
            {
                cout << " ... found." << endl;
                return pDevice;
            }
        }

        cout << "... not found." << endl;

        return nullptr;
    }

}
