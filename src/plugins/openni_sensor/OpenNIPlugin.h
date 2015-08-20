#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include <Astra/Astra.h>
#include <Astra/Plugins/PluginBase.h>
#include <Astra/Plugins/PluginLogger.h>
#include "OniDeviceStream.h"
#include "OniDeviceStreamSet.h"
#include <memory>
#include <vector>
#include <OpenNI.h>
#include <AstraUL/streams/depth_types.h>
#include <AstraUL/streams/color_types.h>
#include <AstraUL/Plugins/stream_types.h>

namespace astra
{
    namespace plugins
    {
        class OpenNIPlugin : public PluginBase,
                             ::openni::OpenNI::DeviceConnectedListener,
                             ::openni::OpenNI::DeviceDisconnectedListener
        {
        public:
            OpenNIPlugin(PluginServiceProxy* pluginService)
                : PluginBase(pluginService, "openni_sensor")
            {
                register_for_host_events();
                init_openni();
            }

            virtual ~OpenNIPlugin();
            virtual void temp_update() override;

            OpenNIPlugin(const OpenNIPlugin&) = delete;
            OpenNIPlugin& operator=(const OpenNIPlugin&) = delete;

        private:
            virtual void on_host_event(astra_event_id id, const void* data, size_t dataSize) override;

            void init_openni();

            virtual void onDeviceConnected(const ::openni::DeviceInfo* info) override;
            virtual void onDeviceDisconnected(const ::openni::DeviceInfo* info) override;

            OniDeviceStreamSet* add_or_get_device(const char* oniUri);
            OniDeviceStreamSet* find_device(const char* oniUri);

            astra_status_t read_streams();

            using SetPtr = std::unique_ptr<OniDeviceStreamSet>;
            using SetList = std::vector<SetPtr>;

            SetList m_sets;
        };
    }
}

#endif /* OPENNIPLUGIN_H */
