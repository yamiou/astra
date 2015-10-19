#include "catch.hpp"
#include <astra/astra.hpp>

TEST_CASE("Can create a StreamSet with default ctor", "[c++-stream-api]") {
    astra::streamset streamset;
    REQUIRE(streamset.is_valid());
}

TEST_CASE("Can create a reader from a streamset", "[c++-stream-api]") {
    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();
    REQUIRE(reader.is_valid());
}


TEST_CASE("Can start depth stream", "[c++-stream-api]") {

    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();
    astra::depthstream depthStream = reader.stream<astra::depthstream>();
    depthStream.start();
    float hFOV = depthStream.horizontalFieldOfView();
    float vFOV = depthStream.verticalFieldOfView();

    REQUIRE(hFOV > 0);
    REQUIRE(vFOV > 0);
}

TEST_CASE("Can start color stream", "[c++-stream-api]") {

    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();

    reader.stream<astra::colorstream>().start();

    float hFOV = reader.stream<astra::colorstream>().horizontalFieldOfView();
    float vFOV = reader.stream<astra::colorstream>().verticalFieldOfView();

    REQUIRE(hFOV > 0);
    REQUIRE(vFOV > 0);
}