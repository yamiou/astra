/* THIS FILE AUTO-GENERATED FROM SenseKit.cpp.lpp. DO NOT EDIT. */
#include "SenseKitContext.h"
#include <Plugins/StreamServiceProxyBase.h>
#include <SenseKitAPI.h>

static sensekit::SenseKitContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_open_frame(sk_stream* stream, sk_frame** frame) {
	return g_Context.open_frame(stream, frame);
}

SENSEKIT_API sensekit_status_t sensekit_close_frame(sk_frame** frame) {
	return g_Context.close_frame(frame);
}

SENSEKIT_API sensekit_status_t sensekit_initialize() {
	return g_Context.initialize();
}

SENSEKIT_API sensekit_status_t sensekit_terminate() {
	return g_Context.terminate();
}

SENSEKIT_END_DECLS
