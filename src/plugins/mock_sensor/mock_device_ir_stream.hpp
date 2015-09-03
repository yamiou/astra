#ifndef MOCK_DEVICE_IR_STREAM_H
#define MOCK_DEVICE_IR_STREAM_H

#include "mock_base_stream.hpp"
#include "mock_infrared_generator.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;

namespace orbbec { namespace mocks {

    class mock_ir_stream : public orbbec::mocks::base_stream
    {
    public:
        mock_ir_stream();
        virtual ~mock_ir_stream();

        virtual device_status on_initialize() override;
        virtual device_status on_read_into(void* dest, std::size_t size, std::int32_t timeout) override;

    private:
        std::unique_ptr<infrared_generator> generator_;
    };
}}

#endif /* MOCK_DEVICE_IR_STREAM_H */
