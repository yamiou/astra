#define CATCH_CONFIG_RUNNER

#include "catch.hpp"
#include "reporters/catch_reporter_teamcity.hpp"
#include <astra_core/astra_core.hpp>

int main( int argc, char* const argv[] )
{
    astra::initialize();

    int result = Catch::Session().run( argc, argv );

    astra::terminate();

    return result;
}
