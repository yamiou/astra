#include "sensekit_core.h"
#include "sensekit_types.h"
#include "SenseKitAL.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API void sensekitSL_initialize(sensekit_context_t* context)
{
    sensekitAL_initialize(context);
}

SENSEKIT_END_DECLS
