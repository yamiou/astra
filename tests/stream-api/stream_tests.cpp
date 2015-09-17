#include "catch.hpp"
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>

TEST_CASE("Can start a DepthStream", "[c++-stream-api-ul]") {
    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();
    reader.stream<astra::DepthStream>().start();
}
