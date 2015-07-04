package com.orbbec.sensekit;

import android.hardware.usb.UsbDevice;

public interface SensorDeviceManagerListener {
    void onOpenAllDevicesCompleted(Iterable<UsbDevice> availableDevices);
    void onOpenDeviceCompleted(UsbDevice device, boolean opened);
}
