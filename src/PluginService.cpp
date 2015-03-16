#include "PluginService.h"

sensekit_status_t sensekit::PluginService::orbbec_stream_register(context_id ctx, stream_type_id id, stream_handle* handle)
{

	return SENSEKIT_STATUS_SUCCESS;
}

sensekit_status_t sensekit::PluginService::orbbec_stream_unregister(stream_handle* handle)
{
	*handle = nullptr;
	return SENSEKIT_STATUS_SUCCESS;
}

sensekit_status_t sensekit::PluginService::orbbec_swap_bin_buffer(stream_handle handle, buffer* old_buf, buffer* new_buf)
{
	return SENSEKIT_STATUS_SUCCESS;
}
