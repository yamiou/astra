#include "astra_registry.hpp"

namespace astra {

    registry::store& registry::get_store()
    {
        static registry::store store_;
        return store_;
    }
}
