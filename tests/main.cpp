#define CATCH_CONFIG_RUNNER

#include "catch.hpp"
#include "reporters/catch_reporter_teamcity.hpp"
#include <Astra/Astra.h>

int main( int argc, char* const argv[] )
{
    astra::Astra::initialize();

    int result = Catch::Session().run( argc, argv );

    astra::Astra::terminate();

    return result;
}
