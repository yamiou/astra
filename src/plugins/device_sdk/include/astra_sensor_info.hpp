#ifndef ASTRA_SENSOR_INFO_H
#define ASTRA_SENSOR_INFO_H

#include <string>

namespace astra { namespace devices {

    enum class sensor_type
    {
        depth,
        color
    };

    class sensor_info
    {
    public:
        sensor_info(const sensor_type& type, const std::string& displayName);

        const sensor_type& type() const;
        const std::string& displayName() const;

    private:
        sensor_type type_;
        std::string displayName_;
    };
}}

#endif /* ASTRA_SENSOR_INFO_H */
