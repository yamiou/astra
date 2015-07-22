#include "catch.hpp"
#include <SenseKit/SenseKit.h>
#include <SenseKitUL/SenseKitUL.h>

TEST_CASE("Can start a DepthStream", "[c++-stream-api-ul]") {
    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();
    reader.stream<sensekit::DepthStream>().start();
}
