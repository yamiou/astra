#ifndef MOCK_STREAM_LISTENER_H
#define MOCK_STREAM_LISTENER_H

namespace orbbec { namespace mocks {

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

#endif /* MOCK_STREAM_LISTENER_H */
