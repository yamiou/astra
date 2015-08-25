#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include <Astra/Astra.h>
#include <Astra/Plugins/PluginBase.h>
#include <Astra/Plugins/PluginLogger.h>
#include "oni_devicestream.hpp"
#include "oni_device_streamset.hpp"
#include <memory>
#include <vector>
#include <OpenNI.h>
#include <AstraUL/streams/depth_types.h>
#include <AstraUL/streams/color_types.h>
#include <AstraUL/Plugins/stream_types.h>

namespace orbbec { namespace ni {

    class oni_adapter_plugin : public astra::PluginBase,
                               public openni::OpenNI::DeviceConnectedListener,
                               public openni::OpenNI::DeviceDisconnectedListener
    {
    public:
        oni_adapter_plugin(astra::PluginServiceProxy* pluginService)
            : PluginBase(pluginService, "openni_sensor")
        {
            register_for_host_events();
            init_openni();
        }

        virtual ~oni_adapter_plugin();
        virtual void temp_update() override;

        oni_adapter_plugin(const oni_adapter_plugin&) = delete;
        oni_adapter_plugin& operator=(const oni_adapter_plugin&) = delete;

    private:
        virtual void on_host_event(astra_event_id id, const void* data, size_t dataSize) override;

        void init_openni();

        virtual void onDeviceConnected(const openni::DeviceInfo* info) override;
        virtual void onDeviceDisconnected(const openni::DeviceInfo* info) override;

        device_streamset* add_or_get_device(const char* oniUri);
        device_streamset* find_device(const char* oniUri);

        astra_status_t read_streams();

        using streamset_ptr = std::unique_ptr<device_streamset>;
        std::vector<streamset_ptr> streamsets_;
    };
}}


#endif /* OPENNIPLUGIN_H */
