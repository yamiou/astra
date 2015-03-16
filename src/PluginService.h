#include "SenseKit-private.h"

namespace sensekit
{
	struct context_id
	{
		int dummy;
	};

	struct stream_type_id
	{
		int type;
	};

	using stream_handle = void*;

	struct stream_data
	{
		context_id context;
		stream_type_id type;
	};

	struct buffer
	{
		unsigned int byteLength;
		void* data;
	};

	using bin_id = void*;

	class PluginService
	{
	public:
		//plugins internally call this on the stream core
		//orbbec_plugin_create_context(...); //unregister

		//metadata = int num_steam_types, stream_type_id[] ids
		//for generators (no requirements, i.e. depth sensor and color sensor) plugin would directly create and register the streams, without using stream_factory
		//orbbec_error orbbec_register_stream_factory(stream_type_id id, transformer_metadata md, stream_factory__callback); //callback gets passed a context_id
		//orbbec_error orbbec_unregister_stream_factory(...); //nominally on plugin shutdown
		//factory is called, plugin creates streams, then calls the stream_register to let the fx know about them
		sensekit_status_t orbbec_stream_register(context_id ctx, stream_type_id id, /*out*/stream_handle* handle);// , stream_antifactory_callback); //I created the stream, I'm letting the core fx know about it
		sensekit_status_t orbbec_stream_unregister(stream_handle* handle); //stream no longer available, nominally on plugin shutdown
		//orbbec_error orbbec_stream_create_bin(stream_handle handle, /*out*/ bin_id& id, unsigned int byte_length ...);
		//orbbec_error orbbec_stream_destroy_bin(stream_handle handle, bin_id id);
		//orbbec_error orbbec_stream_subscribe_client_added_event(stream_handle handle, ...); //and unsubscribe...
		//orbbec_error orbbec_stream_subscribe_client_removed_event(stream_handle handle, ...); //and unsubscribe...
		//orbbec_error orbbec_stream_assign_client_to_bin(stream_handle handle, client_id id, bin_id id);

		sensekit_status_t orbbec_swap_bin_buffer(stream_handle handle, buffer* old_buf, /*out*/buffer* new_buf);
			
		//sensekit_status_t orbbec_stream_swap_bin_buffers(stream_handle handle, /*bin_id id,*/ unsigned int byte_length, void** frontBuffer, void** backBuffer);
		//orbbec_error orbbec_stream_register_get_parameter_callback(component_handle handle, client_id client, ...);
		//orbbec_error orbbec_stream_register_set_parameter_callback(component_handle handle, client_id client, ...);

		//sensekit_status_t 
	};

}