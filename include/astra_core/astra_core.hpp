#ifndef ASTRA_CORE_HPP
#define ASTRA_CORE_HPP

#include "astra_streamset.hpp"
#include "astra_stream_description.hpp"
#include "astra_frame.hpp"
#include "astra_frame_listener.hpp"
#include "astra_stream_reader.hpp"
#include "astra_datastream.hpp"
#include "astra_cxx_make_unique.hpp"

namespace astra {

    inline astra_status_t initialize()
    {
        return astra_initialize();
    }

    inline astra_status_t terminate()
    {
        return astra_terminate();
    }
}

#endif // ASTRA_CORE_HPP
