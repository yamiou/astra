#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include <SenseKit/Plugins/plugin_api.h>
#include "OniDeviceStream.h"
#include <SenseKit/SenseKit.h>
#include <memory>
#include <vector>
#include <OpenNI.h>
#include <SenseKitUL/streams/depth_types.h>
#include <SenseKitUL/streams/color_types.h>
#include "../../SenseKitUL/SenseKitUL_internal.h"
#include <SenseKit/Plugins/StreamCallbackListener.h>

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

            virtual void onDeviceConnected(const ::openni::DeviceInfo* info);
            virtual void onDeviceDisconnected(const ::openni::DeviceInfo* info);

            sensekit_status_t open_sensor_streams();
            sensekit_status_t close_sensor_streams();

            sensekit_status_t read_streams();

            bool m_isDeviceOpen{ false };
            ::openni::Device m_device;
            ::openni::DeviceInfo m_deviceInfo;

            sensekit_streamset_t m_streamSetHandle{nullptr};
            Sensor* m_sensor;

            using StreamPtr = std::unique_ptr<OniDeviceStreamBase>;
            using StreamList = std::vector<StreamPtr>;

            StreamList m_streams;
        };
    }
}

#endif /* OPENNIPLUGIN_H */