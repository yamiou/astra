#include "astra_streamset_connection.hpp"
#include <memory>
#include "astra_cxx_compatibility.hpp"

namespace astra {

    stream_reader* streamset_connection::create_reader()
    {
        ReaderPtr reader = std::make_unique<stream_reader>(*this);
        stream_reader* rawPtr = reader.get();

        readers_.push_back(std::move(reader));
        return rawPtr;
    }

    bool streamset_connection::destroy_reader(astra::stream_reader* reader)
    {
        return true;
    }
}
