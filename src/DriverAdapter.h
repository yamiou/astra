#ifndef DEVICEADAPTER_H
#define DEVICEADAPTER_H

#include <SenseKit.h>
#include "Signal.h"
#include <vector>

namespace sensekit {

    class DriverAdapter;
    class Device;

    struct DeviceConnectedEventArgs
    {
        DriverAdapter* adapter;
        Device* device;

        DeviceConnectedEventArgs(DriverAdapter* a, Device* d)
            : adapter(a), device(d) {}
    };

    struct DeviceDisconnectedEventArgs
    {
        DriverAdapter* adapter;
        Device* device;

        DeviceDisconnectedEventArgs(DriverAdapter* a, Device* d)
            : adapter(a), device(d) {}
    };

    struct DeviceChangedEventArgs
    {
        DriverAdapter* adapter;
        Device* device;

        DeviceChangedEventArgs(DriverAdapter* a, Device* d)
            : adapter(a), device(d) {}

    };

    enum driver_status_t
        {
            DRIVER_STATUS_SUCCESS
        };

    using device_handle_t =  void*;
    using stream_handle_t = void*;

    enum stream_type_t
        {
            SENSEKIT_STREAM_COLOR = 1,
            SENSEKIT_STREAM_DEPTH = 2
        };

    using device_connected_callback_t = void (*)(DriverAdapter*, const sensekit_device_desc_t&, void*);
    using device_disconnected_callback_t = void (*)(const sensekit_device_desc_t&, void* context);
    using device_changed_callback_t = void (*)(const sensekit_device_desc_t&, void*);

    using DeviceList = std::vector<Device*>;

    class DriverAdapter
    {
    public:

        DriverAdapter() {};
        virtual ~DriverAdapter() {}

        virtual sensekit_status_t initialize()
            {
                return SENSEKIT_STATUS_SUCCESS;
            };

        virtual sensekit_status_t terminate() = 0;

        virtual void open_device(Device* device) = 0;

        virtual driver_status_t close_device(Device* device) = 0;

        const DeviceList& get_devices() { return m_devices; }

        virtual driver_status_t get_available_streams(
            device_handle_t deviceHandle,
            const sensekit_stream_desc_t* descArray,
            size_t* count) = 0;

        virtual stream_handle_t open_stream(device_handle_t deviceHandle, stream_type_t steamType) = 0;
        virtual void close_stream(device_handle_t deviceHandle, stream_handle_t streamHandle) = 0;
        virtual sensekit_status_t has_device_for_uri(const char *uri, bool& deviceAvailable) = 0;
        Device* query_for_device(const char* uri);

        Signal<DeviceConnectedEventArgs>& connectedSignal() { return m_connectedSignal; };
        Signal<DeviceDisconnectedEventArgs>& disconnectedSignal() { return m_disconnectedSignal; }
        Signal<DeviceChangedEventArgs>& changedSignal() { return m_changedSignal; }

    protected:

        void raiseConnected(Device* device)
            {
                DeviceConnectedEventArgs eventArgs(this, device);
                m_connectedSignal.raise(eventArgs);
            };

        void raiseDisconnected(Device* device)
            {
                DeviceDisconnectedEventArgs eventArgs(this, device);
                m_disconnectedSignal.raise(eventArgs);
            }

        void raiseChanged(Device* device)
            {
                DeviceChangedEventArgs eventArgs(this, device);
                m_changedSignal.raise(eventArgs);
            }

        void add_device(Device* device);
        bool remove_device(Device* device);
        bool device_exists(Device* device);

        DeviceList m_devices;

        bool find_device_by_uri(const char* uri, Device** device);
        bool find_device_by_id(size_t deviceId, Device** device);

    private:

        Signal<DeviceConnectedEventArgs> m_connectedSignal;
        Signal<DeviceDisconnectedEventArgs> m_disconnectedSignal;
        Signal<DeviceChangedEventArgs> m_changedSignal;

    };
}

#endif /* DEVICEADAPTER_H */
