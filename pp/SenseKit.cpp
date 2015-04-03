/* THIS FILE AUTO-GENERATED FROM SenseKit.cpp.lpp. DO NOT EDIT. */
#include "SenseKitContext.h"
#include <Plugins/StreamServiceProxyBase.h>
#include <SenseKitAPI.h>

static sensekit::SenseKitContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_initialize() {
	return g_Context.initialize();
}

SENSEKIT_API void sensekit_terminate() {
	return g_Context.terminate();
}

SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString, sensekit_streamset_t** streamSet) {
	return g_Context.streamset_open(connectionString, *streamSet);
}

SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t** streamSet) {
	return g_Context.streamset_close(*streamSet);
}

SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status) {
	return g_Context.get_status_string(status);
}

SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t* streamSet, sensekit_reader_t** reader) {
	return g_Context.reader_create(streamSet, *reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t** reader) {
	return g_Context.reader_destroy(*reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t** reader) {
	return g_Context.reader_get_stream(*reader);
}

SENSEKIT_END_DECLS
