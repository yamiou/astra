#ifndef ASTRA_SENSOR_STATE_H
#define ASTRA_SENSOR_STATE_H

namespace astra { namespace devices {

    enum class sensor_state_value
    {
        ready = 0,
        not_ready,
        faulted
    };

    class sensor_state
    {
    public:
        sensor_state() {}

        sensor_state(const sensor_state_value& value)
            : value_(value) {}

        const sensor_state_value& value() const;
        operator bool() const;

    private:
        sensor_state_value value_{sensor_state_value::not_ready};
    };

    bool operator==(const sensor_state& lhs, const sensor_state& rhs);
    bool operator!=(const sensor_state& lhs, const sensor_state& rhs);

}}

#endif /* ASTRA_SENSOR_STATE_H */
