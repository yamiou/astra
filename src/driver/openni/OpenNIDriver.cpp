#include <OpenNI.h>
#include "OpenNIDriver.h"
#include "OpenNIDeviceAdapter.h"

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit { namespace openni {

        void OpenNIDriver::initialize()
        {
            register_for_events();

            ::openni::OpenNI::initialize();

            raise_driver_initialized();
        }

        void OpenNIDriver::terminate()
        {
            raise_driver_terminated();
            unregister_for_events();
        }

        void OpenNIDriver::register_for_events()
        {
            ::openni::OpenNI::addDeviceConnectedListener(&m_listener);
            ::openni::OpenNI::addDeviceDisconnectedListener(&m_listener);
            ::openni::OpenNI::addDeviceStateChangedListener(&m_listener);
        }

        void OpenNIDriver::unregister_for_events()
        {
            ::openni::OpenNI::removeDeviceConnectedListener(&m_listener);
            ::openni::OpenNI::removeDeviceDisconnectedListener(&m_listener);
            ::openni::OpenNI::removeDeviceStateChangedListener(&m_listener);
        }

        void OpenNIDriver::on_device_connected(const ::openni::DeviceInfo* info)
        {
            OpenNIDeviceAdapter* adapter = new OpenNIDeviceAdapter(info);
            //Device* device = new Device(adapter);;

            //cout << "driver: device connected: " << device->get_description().uri << endl;

            //device->on_connected();
            // raise_device_connected(adapter);
        }

        void OpenNIDriver::on_device_disconnected(const ::openni::DeviceInfo* info)
        {
            // Device* device = nullptr;

            // if (find_device_by_uri(info->getUri(), &device))
            // {
            //     cout << "driver: device disconnected: " << device->get_description().uri << endl;
            //     device->on_disconnected();
            //     raise_device_disconnected(device);
            // }
        }

        void OpenNIDriver::on_device_changed(const ::openni::DeviceInfo* info,
                                              ::openni::DeviceState state)
        {

        }



}}
