#include "catch.hpp"
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>

TEST_CASE("Can start a DepthStream", "[c++-stream-api-ul]") {
    astra::Sensor sensor;
    astra::StreamReader reader = sensor.create_reader();
    reader.stream<astra::DepthStream>().start();
}
