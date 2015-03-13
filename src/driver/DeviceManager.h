#ifndef DEVICEMANAGER_H
#define DEVICEMANAGER_H

#include <vector>
#include "../Signal.h"

namespace sensekit {
    class Device;
}

namespace sensekit { namespace driver {

        using DeviceList = std::vector<Device*>;

        struct DeviceConnectedEventArgs
        {
            Device* device;

            DeviceConnectedEventArgs(Device* device)
                : device(device) {}
        };

        struct DeviceDisconnectedEventArgs
        {
            Device* device;

            DeviceDisconnectedEventArgs(Device* device)
                : device(device) {}
        };

        struct DeviceChangedEventArgs
        {
            Device* device;

            DeviceChangedEventArgs(Device* device)
                : device(device) {}
        };

        class DeviceManager
        {
        public:
            DeviceManager();
            virtual ~DeviceManager();

            const DeviceList& get_devices() { return m_devices; }

            Device* query_for_device(const char* uri);

            Signal<DeviceConnectedEventArgs>& connectedSignal()
                { return m_connectedSignal; };

            Signal<DeviceDisconnectedEventArgs>& disconnectedSignal()
                { return m_disconnectedSignal; }

            Signal<DeviceChangedEventArgs>& changedSignal()
                { return m_changedSignal; }

            void on_device_connected(Device* device);
            void on_device_disconnected(Device* device);
            void on_device_changed(Device* device);

        protected:

            void raiseConnected(Device* device)
                {
                    DeviceConnectedEventArgs eventArgs(device);
                    m_connectedSignal.raise(eventArgs);
                };

            void raiseDisconnected(Device* device)
                {
                    DeviceDisconnectedEventArgs eventArgs(device);
                    m_disconnectedSignal.raise(eventArgs);
                }

            void raiseChanged(Device* device)
                {
                    DeviceChangedEventArgs eventArgs(device);
                    m_changedSignal.raise(eventArgs);
                }

            void add_device(Device* device);
            bool remove_device(Device* device);
            bool device_exists(Device* device);

            bool find_device_by_uri(const char* uri, Device** device);
            bool find_device_by_id(size_t deviceId, Device** device);

            DeviceList m_devices;

        private:

            Signal<DeviceConnectedEventArgs> m_connectedSignal;
            Signal<DeviceDisconnectedEventArgs> m_disconnectedSignal;
            Signal<DeviceChangedEventArgs> m_changedSignal;

        };
}}

#endif /* DEVICEMANAGER_H */
