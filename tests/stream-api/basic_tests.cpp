#include "catch.hpp"
#include <Astra/Astra.h>

TEST_CASE("Can create a StreamSet with default ctor", "[c++-stream-api]") {
    astra::StreamSet streamset;
    REQUIRE(streamset.is_valid());
}

TEST_CASE("Can create a reader from a streamset", "[c++-stream-api]") {
    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();
    REQUIRE(reader.is_valid());
}
