#ifndef ASTRA_DEVICE_INFO_H
#define ASTRA_DEVICE_INFO_H

#include <cstdint>
#include <string>

namespace astra { namespace devices {

    class device_info
    {
    public:
        device_info(std::string uri, std::uint32_t vendorId, std::uint32_t productId);

        const std::string& uri() const;
        const std::uint32_t& vendor_id() const;
        const std::uint32_t& product_id() const;

    private:
        std::string uri_;
        std::uint32_t vendorId_{0};
        std::uint32_t productId_{0};
    };

    bool operator==(const device_info& lhs, const device_info& rhs);
    bool operator!=(const device_info& lhs, const device_info& rhs);
}}

#endif /* ASTRA_DEVICE_INFO_H */
