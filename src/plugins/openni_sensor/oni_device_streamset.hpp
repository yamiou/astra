#ifndef ONI_DEVICE_STREAMSET_H
#define ONI_DEVICE_STREAMSET_H

#include <Astra/StreamSet.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/PluginLogger.h>
#include <OpenNI.h>
#include <memory>
#include <vector>
#include <string>
#include "oni_stream.hpp"
#include "oni_stream_listener.hpp"

namespace orbbec { namespace ni {

    class device_streamset : public stream_listener
    {
    public:
        device_streamset(std::string name, astra::PluginServiceProxy& pluginService, const char* uri);
        ~device_streamset();

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
        astra_streamset_t streamSetHandle_;
        openni::Device oniDevice_;
        std::string uri_;

        using stream_ptr = std::unique_ptr<stream>;
        std::vector<stream_ptr> streams_;

        std::vector<openni::VideoStream*> niActiveStreams_;
        std::vector<stream*> astraActiveStreams_;

        astra_frame_index_t frameIndex_{ 0 };
    };
}}

#endif /* ONI_DEVICE_STREAMSET_H */
