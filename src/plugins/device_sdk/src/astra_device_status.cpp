#include "astra_device_status.hpp"

namespace astra { namespace devices {

    const device_status_value& device_status::value() const
    {
        return value_;
    }

    device_status::operator bool() const
    {
        return value() == device_status_value::ok;
    }

    bool operator==(const device_status& lhs, const device_status& rhs)
    {
        return lhs.value() == rhs.value();
    }

    bool operator!=(const device_status& lhs, const device_status& rhs)
    {
        return !(lhs == rhs);
    }

}}
