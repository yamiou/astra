#include "OpenNIPlugin.h"
#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
#include "OniDepthStream.h"
#include "OniColorStream.h"

EXPORT_PLUGIN(sensekit::plugins::OpenNIPlugin);

namespace sensekit
{
    namespace plugins
    {
        void OpenNIPlugin::init_openni()
        {
            openni::Version version = openni::OpenNI::getVersion();

            get_logger().info("Initializing OpenNI v%d.%d.%d.%d",
                 version.major,
                 version.minor,
                 version.maintenance,
                 version.build);

            ::openni::Status rc = ::openni::STATUS_OK;

            ::openni::OpenNI::addDeviceConnectedListener(this);
            ::openni::OpenNI::addDeviceDisconnectedListener(this);

            rc = ::openni::OpenNI::initialize();
        }

        void OpenNIPlugin::onDeviceConnected(const ::openni::DeviceInfo* info)
        {
            get_logger().info("device connected, opening device");
            OniDeviceStreamSet* set = new OniDeviceStreamSet(get_pluginService(), info);

            m_sets.push_back(SetPtr(set));
        }

        void OpenNIPlugin::onDeviceDisconnected(const ::openni::DeviceInfo* info)
        {
            get_logger().info("device disconnected");
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
            get_logger().info("shutting down openni");
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
