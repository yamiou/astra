#ifndef MOCK_DEVICE_DEPTH_STREAM_H
#define MOCK_DEVICE_DEPTH_STREAM_H

#include <AstraUL/astraul_ctypes.h>
#include <AstraUL/streams/depth_capi.h>

#include <cmath>
#include <chrono>

#include "mock_depth_generator.hpp"
#include "astra_simple_timer.hpp"
#include "mock_base_stream.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;

namespace orbbec { namespace mocks {

    class mock_depth_stream : public base_stream
    {
    public:
        mock_depth_stream();
        virtual ~mock_depth_stream();

        virtual device_status on_initialize() override;
        virtual device_status on_read_into(void* dest, std::size_t size, std::int32_t timeout) override;

        virtual device_status on_get_property(property_id id, void* value, std::size_t size) override;
        virtual device_status on_get_property_size(property_id id, std::size_t* size) override;
        virtual device_status on_set_property(property_id id, const void* value, std::size_t size) override;
        virtual bool on_property_supported(property_id id) const override;

        virtual void on_active_mode_changed() override;

    private:
        void refresh_conversion_cache();

        std::unique_ptr<depth_generator> generator_;
        conversion_cache_t conversionCache_;

    };
}}

#endif /* MOCK_DEVICE_DEPTH_STREAM_H */
