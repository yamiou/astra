#include "catch.hpp"
#include <astra_core/astra_core.hpp>
#include <astra/astra.hpp>

TEST_CASE("Can start a depthstream", "[c++-stream-api-ul]") {
    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();
    reader.stream<astra::depthstream>().start();
}
