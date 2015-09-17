#ifndef ONI_STREAM_LISTENER_H
#define ONI_STREAM_LISTENER_H

namespace orbbec { namespace ni {

    class stream;

    class stream_listener
    {
    public:
        virtual ~stream_listener() {}
        virtual void on_opened(stream* stream) {}
        virtual void on_closed(stream* stream) {}
        virtual void on_started(stream* stream) {}
        virtual void on_stopped(stream* stream) {}
    };
}}

#endif /* ONI_STREAM_LISTENER_H */
