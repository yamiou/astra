package com.orbbec.jni;

public class SenseKit {
    static {
        System.loadLibrary("SenseKit");
        System.loadLibrary("SenseKitAPI");
        System.loadLibrary("SenseKitUL");
        System.loadLibrary("sensekit_jni");
    }
    public static native void initialize();
    public static native void terminate();
    public static native void notify_resource_available(String uri);
    public static native void notify_resource_unavailable(String uri);
}
