#ifndef ASTRA_DEVICE_STATE_H
#define ASTRA_DEVICE_STATE_H

namespace astra { namespace devices {

    enum class device_state_value
    {
        not_ready = 0,
        ready,
        faulted
    };

    class device_state
    {
    public:
        device_state();

        device_state(const device_state_value& value)
            : value_(value) {};

        const device_state_value& value() const;
        operator bool() const;

    private:
        device_state_value value_{device_state_value::not_ready};
    };

    bool operator==(const device_state& lhs, const device_state& rhs);
    bool operator!=(const device_state& lhs, const device_state& rhs);
}}


#endif /* ASTRA_DEVICE_STATE_H */
