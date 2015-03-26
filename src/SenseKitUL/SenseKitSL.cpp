#include <SenseKit.h>
#include "SenseKitAL.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API void sensekitSL_initialize(sensekit_context_t* context)
{
    sensekitAL_initialize(context);
}

SENSEKIT_END_DECLS
