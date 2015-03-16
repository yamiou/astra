#include "PluginBase.h"

void sensekit::PluginBase::orbbec_plugin_init(MockContext* context, PluginService* pluginService)
{
	context_id ctx;
	stream_type_id id;
	stream_handle handle;
	
	int depthstuff = 1;
	pluginService->orbbec_stream_register(ctx, id, &handle);
	//depthstuff -> handle




	//depthstuff has data
	//depthstuff -> handle
	buffer frontFrame;
	buffer backFrame;
	pluginService->orbbec_swap_bin_buffer(handle, &frontFrame, &backFrame);

}