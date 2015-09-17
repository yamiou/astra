#include "astra_device_state.hpp"

namespace astra { namespace devices {

    const device_state_value& device_state::value() const
    {
        return value_;
    }

    device_state::operator bool() const
    {
        return value() != device_state_value::faulted;
    }

    bool operator==(const device_state& lhs, const device_state& rhs)
    {
        return lhs.value() == rhs.value();
    }

    bool operator!=(const device_state& lhs, const device_state& rhs)
    {
        return !(lhs == rhs);
    }
}}
