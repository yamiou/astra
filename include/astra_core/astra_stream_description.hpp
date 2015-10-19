#ifndef ASTRA_STREAM_DESCRIPTION_HPP
#define ASTRA_STREAM_DESCRIPTION_HPP

#include "capi/astra_core.h"
#include <cassert>

namespace astra {

    class stream_description : private ::astra_stream_desc_t
    {
    public:
        stream_description(::astra_stream_type_t type,
                           ::astra_stream_subtype_t subtype = DEFAULT_SUBTYPE)
        {
            ::astra_stream_desc_t::type = type;
            ::astra_stream_desc_t::subtype = subtype;
        }

        stream_description(const ::astra_stream_desc_t& desc)
        {
            *this = desc;
        }

        stream_description& operator=(const ::astra_stream_desc_t& desc)
        {
            ::astra_stream_desc_t::type = desc.type;
            ::astra_stream_desc_t::subtype = desc.subtype;

            return *this;
        }

        operator ::astra_stream_desc_t*() { return this; }
        operator const ::astra_stream_desc_t*() const { return this; }

        astra_stream_type_t type() const { return ::astra_stream_desc_t::type; }
        void set_type(astra_stream_type_t type) { ::astra_stream_desc_t::type = type; }

        astra_stream_subtype_t subtype() const { return ::astra_stream_desc_t::subtype; }
        void set_subtype(astra_stream_subtype_t subtype) { ::astra_stream_desc_t::subtype = subtype; }
    };

    inline bool operator==(const stream_description& lhs, const stream_description& rhs)
    {
        return lhs.type() == rhs.type() && lhs.subtype() == rhs.subtype();
    }

    inline bool operator!=(const stream_description& lhs, const stream_description& rhs)
    {
        return !(lhs == rhs);
    }
}

#endif // ASTRA_STREAM_DESCRIPTION_HPP
