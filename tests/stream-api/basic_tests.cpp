#include "catch.hpp"
#include <Astra/Astra.h>

TEST_CASE("Can create a Sensor with default ctor", "[c++-stream-api]") {
    astra::Sensor sensor;
    REQUIRE(sensor.is_valid());
}

TEST_CASE("Can create a reader from a sensor", "[c++-stream-api]") {
    astra::Sensor sensor;
    astra::StreamReader reader = sensor.create_reader();
    REQUIRE(reader.is_valid());
}
