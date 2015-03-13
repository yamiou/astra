#ifndef OPENNIDRIVER_H
#define OPENNIDRIVER_H

#include <OpenNI.h>
#include "../Driver.h"

namespace sensekit { namespace openni {

        class OpenNIDriver : public sensekit::driver::Driver
        {
        public:
            OpenNIDriver() : Driver(), m_listener(*this) {};
            virtual ~OpenNIDriver() {};

            virtual void initialize() override;
            virtual void terminate() override;

        private:
            void register_for_events();
            void unregister_for_events();

            void on_device_connected(const ::openni::DeviceInfo* info);
            void on_device_disconnected(const ::openni::DeviceInfo* info);
            void on_device_changed(const ::openni::DeviceInfo* info, ::openni::DeviceState state);

            class OpenNIEventsListener :
                public ::openni::OpenNI::DeviceConnectedListener,
                public ::openni::OpenNI::DeviceDisconnectedListener,
                public ::openni::OpenNI::DeviceStateChangedListener
            {
            public:

                virtual void onDeviceConnected(const ::openni::DeviceInfo* info) override
                    {
                        m_driver.on_device_connected(info);
                    }

                virtual void onDeviceDisconnected(const ::openni::DeviceInfo* info) override
                    {
                        m_driver.on_device_disconnected(info);
                    }

                virtual void onDeviceStateChanged(const ::openni::DeviceInfo* info,
                                                  ::openni::DeviceState state) override
                    {
                        m_driver.on_device_changed(info, state);
                    }

            private:
                OpenNIDriver& m_driver;

                OpenNIEventsListener(OpenNIDriver& driver) : m_driver(driver) { }

                friend class OpenNIDriver;
            };


            OpenNIEventsListener m_listener;

            friend class OpenNIEventsListener;

        };
    }}

#endif /* OPENNIDRIVER_H */
