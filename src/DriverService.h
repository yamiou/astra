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

        using DeviceList = std::vector<Device*>;

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

        DeviceConnectedSignal m_connectedSignal;
        DeviceDisconnectedSignal m_disconnectedSignal;
        DeviceChangedSignal m_changedSignal;

        void adapter_deviceConnected(DeviceConnectedEventArgs args);
        void adapter_deviceDisconnected(DeviceDisconnectedEventArgs args);
        void adapter_deviceChanged(DeviceChangedEventArgs args);

        bool device_exists(const Device& device);
        Device* find_device_by_id(Device::device_id deviceId);
        Device* find_device_by_uri(const char* uri);
    };
}

#endif /* DRIVERSERVICE_H */
