#include "astra_device_info.hpp"
#include <cassert>

namespace astra { namespace devices {

    device_info::device_info(std::string uri, std::uint32_t vendorId, std::uint32_t productId)
        : uri_(uri), vendorId_(vendorId), productId_(productId)
    {
        assert(uri_.length() > 0);
    }

    const std::string& device_info::uri() const
    {
        return uri_;
    }

    const std::uint32_t& device_info::vendor_id() const
    {
        return vendorId_;
    }

    const std::uint32_t& device_info::product_id() const
    {
        return productId_;
    }

    bool operator==(const device_info& lhs, const device_info& rhs)
    {
        return lhs.uri() ==  rhs.uri();
    }

    bool operator!=(const device_info& lhs, const device_info& rhs)
    {
        return !(lhs == rhs);
    }
}}
