#ifndef ASTRA_DATASTREAM_HPP
#define ASTRA_DATASTREAM_HPP

#include "capi/astra_core.h"
#include <astra_core/astra_stream_description.hpp>
#include <stdexcept>

namespace astra {

    class datastream
    {
    public:
        datastream()
        {}

        datastream(astra_streamconnection_t connection)
            : connection_(connection)
        {
            if(connection_ != nullptr)
            {
                astra_stream_get_description(connection,&description_);
            }
        }

        bool is_available() { return connection_ != nullptr; }

        void start()
        {
            if(connection_ == nullptr)
            {
                throw std::logic_error("Cannot start a stream that is not available");
            }
            astra_stream_start(connection_);
        }
        void stop()

        {
            if(connection_ == nullptr)
            {
                throw std::logic_error("Cannot stop a stream that is not available");
            }
            astra_stream_stop(connection_);
        }

    private:
        astra_streamconnection_t connection_{nullptr};
        astra_stream_desc_t description_;
    };
}

#endif // ASTRA_DATASTREAM_HPP
