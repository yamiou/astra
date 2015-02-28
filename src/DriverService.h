#ifndef DRIVERSERVICE_H
#define DRIVERSERVICE_H

#include <vector>
#include <SenseKit.h>
#include "Signal.h"
#include "DriverAdapter.h"
#include "Device.h"

namespace sensekit {

    class DriverService
    {
    public:

        typedef std::vector<Device*> DeviceList;

        typedef Signal<Device*> DeviceConnectedSignal;
        typedef DeviceConnectedSignal::callback_type DeviceConnectedCallback;
        typedef Signal<Device*> DeviceDisconnectedSignal;
        typedef DeviceDisconnectedSignal::callback_type DeviceDisconnectedCallback;
        typedef Signal<Device*> DeviceChangedSignal;
        typedef DeviceChangedSignal::callback_type DeviceChangedCallback;

        typedef size_t CallbackId;

        DriverService(DriverAdapter& driverAdapter)
            : m_driverAdapter(driverAdapter)
            {}

        ~DriverService() {}

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        const DeviceList& get_devices() { return m_devices; }
        sensekit_status_t query_for_device(const char* uri, Device** device);

        CallbackId registerDeviceConnectedCallback(DeviceConnectedCallback callback)
            { return m_connectedSignal += callback; };
        CallbackId registerDeviceDisconnectedCallback(DeviceDisconnectedCallback callback)
            { return m_disconnectedSignal += callback; };
        CallbackId registerDeviceChangedCallback(DeviceChangedCallback callback)
            { return m_changedSignal += callback; };

        bool unregisterDeviceConnectedCallback(CallbackId callbackId)
            { return m_connectedSignal -= callbackId; };
        bool unregisterDeviceDisconnectedCallback(CallbackId callbackId)
            { return m_disconnectedSignal -= callbackId; };
        bool unregisterDeviceChangedCallback(CallbackId callbackId)
            { return m_changedSignal -= callbackId; };

        sensekit_status_t open_device(const char *uri, Device** device);

    private:

        DriverAdapter& m_driverAdapter;
        DeviceList m_devices;

        DeviceConnectedSignal m_connectedSignal;
        DeviceDisconnectedSignal m_disconnectedSignal;
        DeviceChangedSignal m_changedSignal;

        static void adapter_deviceConnected(DriverAdapter* adapter,const sensekit_device_desc_t& desc, void* context);
        static void adapter_deviceDisconnected(const sensekit_device_desc_t& desc, void* context);
        static void adapter_deviceChanged(const sensekit_device_desc_t& desc, void* context);

        bool device_exists(const Device& device);
        Device* find_device_by_id(Device::device_id deviceId);
        Device* find_device_by_uri(const char* uri);
        bool remove_device(const Device* device);
        bool add_device(DriverAdapter* adapter, const sensekit_device_desc_t& desc);
    };
}

#endif /* DRIVERSERVICE_H */
