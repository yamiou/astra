#ifndef SENSEKIT_H
#define SENSEKIT_H

#ifdef  __cplusplus
# define SENSEKIT_BEGIN_DECLS  extern "C" {
# define SENSEKIT_END_DECLS    }
#else
# define SENSEKIT_BEGIN_DECLS
# define SENSEKIT_END_DECLS
#endif

#ifndef SENSEKIT_API
# if defined (_MSC_VER) && ! defined (SENSEKIT_WIN32_STATIC_BUILD)
#  define SENSEKIT_API __declspec(dllimport)
# else
#  define SENSEKIT_API
# endif
#endif

SENSEKIT_BEGIN_DECLS

typedef enum _sensekit_status {
	SENSEKIT_STATUS_SUCCESS = 0,
	SENSEKIT_STATUS_INVALID_PARAMETER = 1,
	SENSEKIT_STATUS_DEVICE_ERROR = 2
} sensekit_status_t;

typedef struct _sensekit_sensor sensekit_sensor_t;
typedef struct _sensekit_depthstream sensekit_depthstream_t;
typedef struct _sensekit_depthframe sensekit_depthframe_t;

SENSEKIT_API sensekit_status_t sensekit_open_sensor(char* connection_string, /*out*/ sensekit_sensor_t** sensor);
SENSEKIT_API sensekit_status_t sensekit_close_sensor(sensekit_sensor_t * sensor);
SENSEKIT_API char * sensekit_get_status_string(sensekit_status_t status);

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream);
SENSEKIT_API sensekit_status_t sensekit_depth_close(sensekit_depthstream_t** stream);

SENSEKIT_API sensekit_status_t sensekit_depth_open_frame(sensekit_depthstream_t* stream, int timeout_milliseconds, sensekit_depthframe_t** frame); //0 = return immediately
SENSEKIT_API sensekit_status_t sensekit_depth_close_frame(sensekit_depthframe_t** frame); //frame set to null

SENSEKIT_END_DECLS

#endif // SENSEKIT_H
