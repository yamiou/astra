#include "StreamSetConnection.h"
#include <memory>

namespace sensekit {

    StreamReader* StreamSetConnection::create_reader()
    {
        ReaderPtr reader = std::make_unique<StreamReader>(*this);
        StreamReader* rawPtr = reader.get();

        m_readers.push_back(std::move(reader));
        return rawPtr;
    }

    bool StreamSetConnection::destroy_reader(sensekit::StreamReader* reader)
    {
        return true;
    }
}
