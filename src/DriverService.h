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
        sensekit_status_t destroy();
        const DeviceList& get_devices() { return m_devices; }

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
    private:

        DriverAdapter& m_driverAdapter;
        DeviceList m_devices;

        DeviceConnectedSignal m_connectedSignal;
        DeviceDisconnectedSignal m_disconnectedSignal;
        DeviceChangedSignal m_changedSignal;

        static void adapter_deviceConnected(DriverAdapter* adapter, device_handle_t deviceHandle, void* context);
        static void adapter_deviceDisconnected(device_handle_t deviceHandle, void* context);
        static void adapter_deviceChanged(device_handle_t deviceHandle, void* context);

        bool device_exists(const Device& device);
        Device* find_device(Device::DeviceId deviceId);
        bool remove_device(const Device* device);
    };
}

#endif /* DRIVERSERVICE_H */
