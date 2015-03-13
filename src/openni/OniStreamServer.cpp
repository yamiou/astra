#include "OniStreamServer.h"

#include <iostream>

using std::cout;
using std::endl;

namespace sensekit { namespace openni {

        void OniStreamServer::on_initialize()
        {
            ::openni::OpenNI::initialize();
            cout << "oni stream server: initialized" << endl;
        }

        void OniStreamServer::on_terminate()
        {
            ::openni::OpenNI::shutdown();
            cout << "oni stream server: shutdown." << endl;
        }

        void OniStreamServer::on_device_connected(const ::openni::DeviceInfo* info)
        {

        }

        void OniStreamServer::on_device_disconnected(const ::openni::DeviceInfo* info)
        {

        }

        void OniStreamServer::on_device_changed(const ::openni::DeviceInfo* info,
                                                ::openni::DeviceState state)
        {

        }
    }}
