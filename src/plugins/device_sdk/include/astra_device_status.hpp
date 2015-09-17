#ifndef ASTRA_DEVICE_STATUS_H
#define ASTRA_DEVICE_STATUS_H

namespace astra { namespace devices {

    enum class device_status_value
    {
        ok = 0,
        invalid_parameter,
        not_ready,
        device_error
    };

    class device_status
    {
    public:
        device_status() {}

        device_status(const device_status_value& value)
            : value_(value) {}

        const device_status_value& value() const;
        operator bool() const;

    private:
        device_status_value value_{device_status_value::ok};
    };

    bool operator==(const device_status& lhs, const device_status& rhs);
    bool operator!=(const device_status& lhs, const device_status& rhs);
}}


#endif /* ASTRA_DEVICE_STATUS_H */
