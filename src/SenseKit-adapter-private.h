#ifndef SENSEKIT_ADAPTER_PRIVATE_H
#define SENSEKIT_ADAPTER_PRIVATE_H

struct sensor_handle_t;
typedef struct sensor_handle_t sensor_handle_t;

struct _sensekit_sensor_adapter {
	sensor_handle_t* handle;
	void(*initialize)(struct _sensekit_sensor_adapter * adapter);
	void(*destroy)(struct _sensekit_sensor_adapter * adapter);
};

#endif // SENSEKIT_ADAPTER_PRIVATE_H

