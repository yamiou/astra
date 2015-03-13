#ifndef DRIVER_H
#define DRIVER_H

#include "../Signal.h"
#include "DriverEventListener.h"
#include "DeviceManager.h"

namespace sensekit {
    class Device;
}

namespace sensekit { namespace driver {

        class Driver
        {
        public:
            Driver() :
                  m_deviceManager(),
                  m_initialized(false)
                {
                    m_deviceManager = new DeviceManager();
                }

            virtual ~Driver() {};

            virtual void initialize()
                {
                    initialize_impl();
                    m_initialized = true;
                };

            virtual void terminate()
                {
                    terminate_impl();
                    m_initialized = false;
                }

            virtual void initialize_impl() {}
            virtual void terminate_impl() {}

            DeviceManager* get_device_manager();

        protected:
            inline void raise_driver_initialized()
                {
                    m_driverInitializedSignal.raise();
                }

            inline void raise_driver_terminated()
                {
                    m_driverTerminatedSignal.raise();
                }

            inline void raise_device_connected(Device* device)
                {
                    m_deviceManager->on_device_connected(device);
                    m_deviceConnectedSignal.raise(device);
                }

            inline void raise_device_disconnected(Device* device)
                {
                    m_deviceManager->on_device_disconnected(device);
                    m_deviceDisconnectedSignal.raise(device);
                }

            inline void raise_device_changed(Device* device)
                {
                    m_deviceManager->on_device_changed(device);
                    m_deviceChangedSignal.raise(device);
                }

        private:

            Signal<Device*> m_deviceConnectedSignal;
            Signal<Device*> m_deviceDisconnectedSignal;
            Signal<Device*> m_deviceChangedSignal;
            Signal<void> m_driverInitializedSignal;
            Signal<void> m_driverTerminatedSignal;

            DeviceManager* m_deviceManager;
            bool m_initialized;
        };
}}


#endif /* DRIVER_H */
