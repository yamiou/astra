#include "astra_sensor_state.hpp"

namespace astra { namespace devices {

    const sensor_state_value& sensor_state::value() const
    {
        return value_;
    }

    sensor_state::operator bool() const
    {
        return value() != sensor_state_value::faulted;
    }

    bool operator==(const sensor_state& lhs, const sensor_state& rhs)
    {
        return lhs.value() == rhs.value();
    }

    bool operator!=(const sensor_state& lhs, const sensor_state& rhs)
    {
        return !(lhs == rhs);
    }

}}