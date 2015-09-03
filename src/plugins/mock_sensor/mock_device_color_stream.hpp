#ifndef MOCK_DEVICE_COLOR_STREAM_H
#define MOCK_DEVICE_COLOR_STREAM_H

#include <memory>
#include <chrono>

#include "mock_base_stream.hpp"
#include "mock_color_generator.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;

namespace orbbec { namespace mocks {

    class mock_color_stream : public orbbec::mocks::base_stream
    {
    public:
        mock_color_stream();
        virtual ~mock_color_stream();

        virtual device_status on_initialize() override;
        virtual device_status on_read_into(void* dest, std::size_t size, std::int32_t timeout) override;

    private:
        std::unique_ptr<color_generator> generator_;
    };
}}

#endif /* MOCK_DEVICE_COLOR_STREAM_H */
