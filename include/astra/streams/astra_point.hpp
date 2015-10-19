#ifndef ASTRA_POINT_HPP
#define ASTRA_POINT_HPP

#include <astra_core/astra_core.hpp>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/point_capi.h>
#include <astra/streams/astra_image.hpp>

namespace astra {

    class pointstream : public datastream
    {
    public:
        pointstream()
        {}

        explicit pointstream(astra_streamconnection_t connection)
            : datastream(connection)
        {
            pointStream_ = reinterpret_cast<astra_pointstream_t>(connection);
        }

        static const astra_stream_type_t id = ASTRA_STREAM_POINT;

    private:
        astra_pointstream_t pointStream_;
    };

    class pointframe : public imageframe<vector3f, ASTRA_STREAM_POINT>
    {
    public:
        pointframe(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_POINT)
        {}
    };
}

#endif // ASTRA_POINT_HPP
