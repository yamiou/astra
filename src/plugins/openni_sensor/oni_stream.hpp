#ifndef ONI_STREAM_H
#define ONI_STREAM_H

#include <Astra/Astra.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/Stream.h>
#include <Astra/Plugins/StreamBin.h>
#include <OpenNI.h>
#include <Shiny.h>

namespace orbbec { namespace ni {

    class stream : public astra::plugins::Stream
    {
    public:
        stream(astra::PluginServiceProxy& pluginService,
                   astra_streamset_t streamSet,
                   astra::StreamDescription desc)
            : Stream(pluginService,
                     streamSet,
                     desc)
        {
            PROFILE_FUNC();
        }

        virtual astra_status_t read_frame(astra_frame_index_t frameIndex) = 0;
        virtual astra_status_t open() = 0;
        virtual astra_status_t close() = 0;
        virtual openni::VideoStream* get_stream() = 0;
        virtual astra_status_t start() = 0;
        virtual astra_status_t stop() = 0;

        bool is_open() const { return isOpen_; }
        bool is_started() const { return isStarted_; }

    protected:
        void set_open(bool isOpen) { isOpen_ = isOpen; }
        void set_started(bool isStarted) { isStarted_ = isStarted; }
    private:
        bool isOpen_{false};
        bool isStarted_{false};
    };
}}

#endif /* ONI_STREAM_H */
