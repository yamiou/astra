#ifndef ASTRA_STREAM_STATE_H
#define ASTRA_STREAM_STATE_H

namespace astra { namespace devices {

    enum class stream_state_value
    {
        ready = 0,
        not_ready,
        faulted
    };

    class stream_state
    {
    public:
        stream_state() {}

        stream_state(const stream_state_value& value)
            : value_(value) {}

        const stream_state_value& value() const;
        operator bool() const;

    private:
        stream_state_value value_{stream_state_value::not_ready};
    };

    bool operator==(const stream_state& lhs, const stream_state& rhs);
    bool operator!=(const stream_state& lhs, const stream_state& rhs);
}}

#endif /* ASTRA_STREAM_STATE_H */
