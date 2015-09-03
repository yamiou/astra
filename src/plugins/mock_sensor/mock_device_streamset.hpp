#ifndef MOCK_DEVICE_STREAMSET_H
#define MOCK_DEVICE_STREAMSET_H

#include <Astra/Sensor.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/PluginLogger.h>

#include <functional>
#include <memory>
#include <vector>
#include <string>
#include <chrono>

#include "mock_stream.hpp"
#include "mock_stream_listener.hpp"
#include "astra_device.hpp"

namespace orbbec { namespace mocks {

    class device_streamset : public stream_listener
    {
    public:
        device_streamset(const std::string& name,
                         astra::PluginServiceProxy& pluginService,
                         const char* uri);

        virtual ~device_streamset();

        astra_status_t open();
        astra_status_t close();
        astra_status_t read();

        std::string get_uri() { return uri_; }

        virtual void on_started(stream* stream) override;
        virtual void on_stopped(stream* stream) override;

        device_streamset(const device_streamset&) = delete;
        device_streamset& operator=(const device_streamset&) = delete;

    private:
        bool isOpen_{false};

        astra_status_t open_sensor_streams();
        astra_status_t close_sensor_streams();
        void add_stream(stream* stream);

        astra::PluginServiceProxy& pluginService_;
        std::unique_ptr<astra::Sensor> sensor_;

        astra_streamset_t streamSetHandle_;

        std::string uri_;

        astra::devices::device::shared_ptr device_;

        using stream_ptr = std::unique_ptr<stream>;
        std::vector<stream_ptr> streams_;

        astra_frame_index_t frameIndex_{0};
    };
}}

#endif /* MOCK_DEVICE_STREAMSET_H */
