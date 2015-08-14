#include "catch.hpp"
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>

TEST_CASE("Can create a StreamSet with default ctor", "[c++-stream-api]") {
    astra::StreamSet streamset;
    REQUIRE(streamset.is_valid());
}

TEST_CASE("Can create a reader from a streamset", "[c++-stream-api]") {
    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();
    REQUIRE(reader.is_valid());
}


TEST_CASE("Can start depth stream", "[c++-stream-api]") {

    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();
    astra::DepthStream depthStream = reader.stream<astra::DepthStream>();
    depthStream.start();
    float hFOV = depthStream.horizontalFieldOfView();
    float vFOV = depthStream.verticalFieldOfView();

    REQUIRE(hFOV > 0);
    REQUIRE(vFOV > 0);
}

TEST_CASE("Can start color stream", "[c++-stream-api]") {

    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();

    reader.stream<astra::ColorStream>().start();

    float hFOV = reader.stream<astra::ColorStream>().horizontalFieldOfView();
    float vFOV = reader.stream<astra::ColorStream>().verticalFieldOfView();

    REQUIRE(hFOV > 0);
    REQUIRE(vFOV > 0);
}