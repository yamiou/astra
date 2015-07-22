#define CATCH_CONFIG_RUNNER

#include "catch.hpp"
#include "reporters/catch_reporter_teamcity.hpp"
#include <SenseKit/SenseKit.h>

int main( int argc, char* const argv[] )
{
    sensekit::SenseKit::initialize();

    int result = Catch::Session().run( argc, argv );

    sensekit::SenseKit::terminate();

    return result;
}
