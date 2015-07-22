#include "catch.hpp"
#include <SenseKit/SenseKit.h>

TEST_CASE("Can create a Sensor with default ctor", "[c++-stream-api]") {
    sensekit::Sensor sensor;
    REQUIRE(sensor.is_valid());
}

TEST_CASE("Can create a reader from a sensor", "[c++-stream-api]") {
    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();
    REQUIRE(reader.is_valid());
}
