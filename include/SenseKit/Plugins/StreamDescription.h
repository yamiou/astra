#ifndef STREAMDESCRIPTION_H
#define STREAMDESCRIPTION_H
#include <sensekit_types.h>

namespace sensekit {
    namespace plugins {

        class StreamDescription
        {
        public:
            StreamDescription(sensekit_stream_type_t type, sensekit_stream_subtype_t subtype) :
                type(type),
                subtype(subtype)
            { }

            sensekit_stream_type_t type;
            sensekit_stream_subtype_t subtype;

            friend bool operator==(const StreamDescription& lhs, const StreamDescription& rhs);
            friend bool operator!=(const StreamDescription& lhs, const StreamDescription& rhs);

            inline sensekit_stream_desc_t toDesc()
            {
                sensekit_stream_desc_t desc;
                desc.type = type;
                desc.subType = subtype;
                return desc;
            }

            static StreamDescription fromDesc(sensekit_stream_desc_t& desc)
            {
                return StreamDescription(desc.type, desc.subType);
            }
        };


        inline bool operator==(const StreamDescription& lhs, const StreamDescription& rhs)
        {
            return lhs.type == rhs.type && lhs.subtype == rhs.subtype;
        }

        inline bool operator!=(const StreamDescription& lhs, const StreamDescription& rhs)
        {
            return !(lhs == rhs);
        }

    }
}

#endif /* STREAMDESCRIPTION_H */