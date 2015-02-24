#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "../Signal.h"

TEST_CASE("Can add to a callback list", "[signal]") {
    CallbackList<void, int> cbList;
    size_t id = cbList.add([] (int v) { /* do nothing */ });
    id = cbList.add([] (int v) { /* do nothing */ });
    cbList.add([] (int v) { /* do nothing */ });

    cbList.remove(id);
    REQUIRE(cbList.debug_count() == 2);
}

TEST_CASE("Can add a slot to a signal", "[signal]") {
    Signal<std::string> signal;
    size_t id = signal += [] (std::string v) { };

    REQUIRE(id != size_t(NULL));
}

TEST_CASE("Can raise signal", "[signal]") {
    Signal<int> signal;
    int test = 0;
    signal += [&test] (int v) { test++; };
    signal.raise(1);

    REQUIRE(test == 1);
}

TEST_CASE("Can raise two signals", "[signal]") {
    Signal<int> signal;
    int test = 0;
    signal += [&test] (int v) { test++; };
    signal += [&test] (int v) { test+=2; };
    signal.raise(1);
    REQUIRE(test == 3);
}

TEST_CASE("Can raise void signal", "[signal]") {
    Signal<void> signal;
    int test = 0;
    signal += [&test] { test++; };

    signal.raise();
    REQUIRE(test == 1);
    signal.raise();
}
