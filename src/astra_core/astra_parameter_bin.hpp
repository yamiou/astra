#ifndef ASTRA_PARAMETER_BIN_H
#define ASTRA_PARAMETER_BIN_H

#include <memory>
#include <astra_core/capi/astra_types.h>
#include <cstring>

namespace astra {

    class parameter_bin
    {
    public:
        parameter_bin(size_t byteSize)
            : byteLength_(byteSize)
        {
            //TODO pooling
            data_ = DataPtr(new uint8_t[byteSize]);
	    std::memset(data_.get(), 0, byteSize);
        }

        size_t byteLength() { return byteLength_; }
        void* data() { return data_.get(); }

        astra_parameter_bin_t get_handle() { return reinterpret_cast<astra_parameter_bin_t>(this); }
        static parameter_bin* get_ptr(astra_parameter_bin_t bin)
        {
            return reinterpret_cast<parameter_bin*>(bin);
        }

    private:
        using DataPtr = std::unique_ptr<uint8_t[]>;

        DataPtr data_;
        size_t byteLength_;
    };
}

#endif /* ASTRA_PARAMETER_BIN_H */
