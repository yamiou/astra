package com.orbbec.jni;

public class Astra {
    static {
        System.loadLibrary("astra");
        System.loadLibrary("astra_core");
        System.loadLibrary("astra_core_api");
        System.loadLibrary("astra_jni");
    }
    public static native void initialize();
    public static native void terminate();
    public static native void notify_resource_available(String uri);
    public static native void notify_resource_unavailable(String uri);
}
