#include "Stream.h"
#include "StreamSource.h"

namespace sensekit {

    void Stream::open()
    {
        m_source.open_stream(this);
    }

    void Stream::close()
    {

    }

}
