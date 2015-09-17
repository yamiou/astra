#include "astra_stream_state.hpp"

namespace astra { namespace devices {

    const stream_state_value& stream_state::value() const
    {
        return value_;
    }

    stream_state::operator bool() const
    {
        return value() != stream_state_value::faulted;
    }

    bool operator==(const stream_state& lhs, const stream_state& rhs)
    {
        return lhs.value() == rhs.value();
    }

    bool operator!=(const stream_state& lhs, const stream_state& rhs)
    {
        return !(lhs == rhs);
    }

}}
