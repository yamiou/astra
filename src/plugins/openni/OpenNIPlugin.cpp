#include "OpenNIPlugin.h"
#include "OniDepthStream.h"
#include "OniColorStream.h"
#include <iostream>
#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
#include "../../SenseKit/sensekit_internal.h"

using std::cout;
using std::endl;

EXPORT_PLUGIN(sensekit::plugins::OpenNIPlugin);

namespace sensekit
{
    namespace plugins
    {
        void OpenNIPlugin::init_openni()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            ::openni::OpenNI::addDeviceConnectedListener(this);
            ::openni::OpenNI::addDeviceDisconnectedListener(this);

            cout << "Initializing openni" << endl;
            rc = ::openni::OpenNI::initialize();
        }

        void OpenNIPlugin::onDeviceConnected(const ::openni::DeviceInfo* info)
        {
            cout << "device connected, opening device" << endl;
            OniDeviceStreamSet* set = new OniDeviceStreamSet(get_pluginService(), info);

            m_sets.push_back(SetPtr(set));
        }

        void OpenNIPlugin::onDeviceDisconnected(const ::openni::DeviceInfo* info)
        {
            cout << "device disconnected" << endl;
            auto it = std::find_if(m_sets.begin(), m_sets.end(),
                                   [&info] (SetPtr& setPtr)
                                   -> bool
                                   {
                                       return setPtr->get_uri() == info->getUri();
                                   });

            m_sets.erase(it);
        }

        OpenNIPlugin::~OpenNIPlugin()
        {
            m_sets.clear();
            cout << "shutting down openni" << endl;
            ::openni::OpenNI::shutdown();
        }


        void OpenNIPlugin::temp_update()
        {
            read_streams();
        }

        sensekit_status_t OpenNIPlugin::read_streams()
        {
            for(auto& set : m_sets)
            {
                set->read();
            }

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}
