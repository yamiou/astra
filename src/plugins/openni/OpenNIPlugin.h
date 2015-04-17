#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/PluginKit.h>
#include "OniDeviceStream.h"
#include "OniDeviceStreamSet.h"
#include <memory>
#include <vector>
#include <OpenNI.h>
#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/streams/color_types.h>
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit
{
    namespace plugins
    {
        class OpenNIPlugin : public PluginBase,
                             ::openni::OpenNI::DeviceConnectedListener,
                             ::openni::OpenNI::DeviceDisconnectedListener
        {
        public:
            OpenNIPlugin(PluginServiceProxy* pluginService)
                : PluginBase(pluginService)
                {
                    init_openni();
                }

            virtual ~OpenNIPlugin();
            virtual void temp_update() override;

            OpenNIPlugin(const OpenNIPlugin&) = delete;
            OpenNIPlugin& operator=(const OpenNIPlugin&) = delete;

        private:
            void init_openni();

            virtual void onDeviceConnected(const ::openni::DeviceInfo* info) override;
            virtual void onDeviceDisconnected(const ::openni::DeviceInfo* info) override;

            sensekit_status_t read_streams();

            using SetPtr = std::unique_ptr<OniDeviceStreamSet>;
            using SetList = std::vector<SetPtr>;

            SetList m_sets;
        };
    }
}

#endif /* OPENNIPLUGIN_H */