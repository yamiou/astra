#ifndef STREAMDESCRIPTION_H
#define STREAMDESCRIPTION_H

#include <SenseKit/sensekit_capi.h>

namespace sensekit {

    class StreamDescription
    {
    public:
        StreamDescription(sensekit_stream_type_t type,
                          sensekit_stream_subtype_t subtype = DEFAULT_SUBTYPE)
            {
                m_desc.type = type;
                m_desc.subtype = subtype;
            }

        StreamDescription(const sensekit_stream_desc_t& desc)
            : m_desc(desc)
            { }

        StreamDescription operator=(const sensekit_stream_desc_t& desc)
            {
                return StreamDescription(desc);
            }

        const sensekit_stream_desc_t& get_desc_t() const { return m_desc; }

        sensekit_stream_type_t get_type() const { return m_desc.type; }
        sensekit_stream_subtype_t get_subtype() const { return m_desc.subtype; }

    private:
        sensekit_stream_desc_t m_desc;
    };

    inline bool operator==(const StreamDescription& lhs, const StreamDescription& rhs)
    {
        return lhs.get_type() == rhs.get_type() && lhs.get_subtype() == rhs.get_subtype();
    }

    inline bool operator!=(const StreamDescription& lhs, const StreamDescription& rhs)
    {
        return !(lhs == rhs);
    }
}

#endif /* STREAMDESCRIPTION_H */
