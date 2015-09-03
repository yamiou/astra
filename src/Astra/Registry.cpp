#include "Registry.h"

namespace astra {

    Registry::store& Registry::get_store()
    {
        static Registry::store store_;
        return store_;
    }
}
