#include "astra_sensor_info.hpp"

namespace astra { namespace devices {

    sensor_info::sensor_info(const sensor_type& type, const std::string& displayName)
        : type_(type), displayName_(displayName)
    {
    }

    const sensor_type& sensor_info::type() const
    {
        return type_;
    }

    const std::string& sensor_info::displayName() const
    {
        return displayName_;
    }
}}
