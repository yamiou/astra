#include "Registry.h"

namespace sensekit {

    Registry::store& Registry::get_store()
    {
        static Registry::store store_;
        return store_;
    }

}
