#ifndef MOCKSENSORPLUGIN_H
#define MOCKSENSORPLUGIN_H

#include <Astra/Astra.h>
#include <Astra/Plugins/PluginBase.h>
#include <Astra/Plugins/PluginLogger.h>
#include "mock_device_stream.hpp"
#include "mock_device_streamset.hpp"
#include <memory>
#include <vector>
#include <AstraUL/streams/depth_types.h>
#include <AstraUL/streams/color_types.h>
#include <AstraUL/Plugins/stream_types.h>

namespace orbbec { namespace mocks {

    class mock_sensor_plugin : public astra::PluginBase
    {
    public:
        mock_sensor_plugin(astra::PluginServiceProxy* pluginService)
            : PluginBase(pluginService, "mock_sensor")
        {
            register_for_host_events();

            add_or_get_device("mock_sensor");
        }

        virtual ~mock_sensor_plugin();
        virtual void temp_update() override;

        mock_sensor_plugin(const mock_sensor_plugin&) = delete;
        mock_sensor_plugin& operator=(const mock_sensor_plugin&) = delete;

    private:
        virtual void on_host_event(astra_event_id id, const void* data, size_t dataSize) override;

        device_streamset* add_or_get_device(const char* testUri);
        device_streamset* find_device(const char* testUri);

        astra_status_t read_streams();

        using streamset_ptr = std::unique_ptr<device_streamset>;
        std::vector<streamset_ptr> streamsets_;
    };
}}


#endif /* MOCKSENSORPLUGIN_H */
