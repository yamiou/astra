#include <jni.h>
#include <Astra/Astra.h>
#include <Astra/host_events.h>
#include "astra_wrapper.h"

#ifdef __cplusplus
extern "C" {
#endif

    JNIEXPORT void JNICALL Java_com_orbbec_jni_Astra_initialize(JNIEnv* env, jclass cls)
    {
        astra_initialize();
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_Astra_terminate(JNIEnv* env, jclass cls)
    {
        astra_terminate();
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_Astra_notify_1resource_1available(JNIEnv* env,
                                                                                    jclass cls,
                                                                                    jstring uri)
    {
        const char* uriString = env->GetStringUTFChars(uri, NULL);
        astra_notify_resource_available(uriString);
        env->ReleaseStringUTFChars(uri, uriString);
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_Astra_notify_1resource_1unavailable(JNIEnv* env,
                                                                                      jclass cls,
                                                                                      jstring uri)
    {
        const char* uriString = env->GetStringUTFChars(uri, NULL);
        astra_notify_resource_unavailable(uriString);
        env->ReleaseStringUTFChars(uri, uriString);
    }

#ifdef __cplusplus
}
#endif
