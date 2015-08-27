#ifndef INFRARED_H
#define INFRARED_H

#include <Astra/Astra.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/streams/infrared_capi.h>
#include <AstraUL/streams/Image.h>
#include <AstraUL/Vector.h>

namespace astra {

    class InfraredStream : public ImageStream
    {
    public:
        explicit InfraredStream(astra_streamconnection_t connection)
            : ImageStream(connection),
              m_infraredStream(reinterpret_cast<astra_infraredstream_t>(connection)),
              m_coordinateMapper(reinterpret_cast<astra_infraredstream_t>(connection))
        { }

        static const astra_stream_type_t id = ASTRA_STREAM_INFRARED;

    private:
        astra_infraredstream_t m_infraredStream;
    };

    class InfraredFrame : public ImageFrame<int16_t, ASTRA_STREAM_INFRARED>
    {
    public:
        InfraredFrame(astra_imageframe_t frame)
            : ImageFrame(frame)
        {}

    };
}

#endif /* INFRARED_H */
