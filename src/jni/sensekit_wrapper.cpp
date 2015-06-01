#include <jni.h>
#include <SenseKit/SenseKit.h>
#include <SenseKit/host_events.h>
#include "com_orbbec_jni_SenseKit.h"

#ifdef __cplusplus
extern "C" {
#endif

    JNIEXPORT void JNICALL Java_com_orbbec_jni_SenseKit_initialize(JNIEnv* env, jclass cls)
    {
        sensekit_initialize();
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_SenseKit_terminate(JNIEnv* env, jclass cls)
    {
        sensekit_terminate();
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_SenseKit_notify_1resource_1available(JNIEnv* env, jclass cls, jstring uri)
    {
        const char* uriString = (*env)->GetStringUTFChars(env, uri, NULL);
        sensekit_notify_resource_available(uriString);
        (*env)->ReleaseStringUTFChars(env, uri, uriString);
    }

    JNIEXPORT void JNICALL Java_com_orbbec_jni_SenseKit_notify_1resource_1unavailable(JNIEnv* env, jclass cls, jstring uri)
    {
        const char* uriString = (*env)->GetStringUTFChars(env, uri, NULL);
        sensekit_notify_resource_unavailable(uriString);
        (*env)->ReleaseStringUTFChars(env, uri, uriString);
    }

#ifdef __cplusplus
}
#endif
