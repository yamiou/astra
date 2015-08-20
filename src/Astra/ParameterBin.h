#ifndef PARAMETERBIN_H
#define PARAMETERBIN_H
#include <memory>
#include <Astra/astra_types.h>

namespace astra {

    class ParameterBin
    {
    public:
        ParameterBin(size_t byteSize)
            : m_byteLength(byteSize)
        {
            //TODO pooling
            m_data = DataPtr(new uint8_t[byteSize]);
            memset(m_data.get(), 0, byteSize);
        }

        size_t byteLength() { return m_byteLength; }
        void* data() { return m_data.get(); }

        astra_parameter_bin_t get_handle() { return reinterpret_cast<astra_parameter_bin_t>(this); }
        static ParameterBin* get_ptr(astra_parameter_bin_t bin)
        {
            return reinterpret_cast<ParameterBin*>(bin);
        }

    private:
        using DataPtr = std::unique_ptr<uint8_t[]>;

        DataPtr m_data;
        size_t m_byteLength;
    };
}

#endif /* PARAMETERBIN_H */
