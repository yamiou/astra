#ifndef ASTRA_FRAME_LISTENER_HPP
#define ASTRA_FRAME_LISTENER_HPP

namespace astra {

    class stream_reader;
    class frame;

    class frame_listener
    {
    public:
        virtual ~frame_listener() = default;
        virtual void on_frame_ready(stream_reader& reader, frame& frame) = 0;
    };

    inline bool operator==(const frame_listener& l, const frame_listener& r)
    {
        return &l == &r;
    }
}

#endif // ASTRA_FRAME_LISTENER_HPP
