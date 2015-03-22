int fatalErrorLevel = 255;

SensorContext* context = acquireSensorContext();

if (sensorCount(context) == 0) {
	return fatalErrorLevel;
}

Sensor* sensor;
API_RESULT result = sensorGetDefault(context, &sensor);

if (sensorError(result)) {
	return fatalErrorLevel;
}

if ((sensorStatus(sensor) & SENSOR_AVAILABLE) != SENSOR_AVAILABLE) {
	return fatalErrorLevel;
}

if (sensorError(sensorConnect(sensor))) {
	return fatalErrorLevel;
}

// use imagination for error checking from here forward

HandTracker* tracker;
API_RESULT result = handTrackerCreate(sensor, &tracker);
handTrackerStart(tracker);

StreamHandle handStream;
handTrackerGetStreamHandle(tracker, handStream)

while (true) {
	HandFrame* handFrame;

	streamAcquireFrame(handStream, &handFrame, 500);

	if (handFrame == NULL)
	{
		continue;
	}

	int count = handFrameGetHandCount(handFrame);
	for(int i = 0; i < handFrame.handCount; i++)
	{
		HandInfo handInfo = handFrameGetHandInfo(handFrame, i);
		printf("%f, %f, %f", handInfo.position.x, handInfo.position.y, handInfo.position.z);
	}

	streamReleaseFrame(handStream, handFrame);
}

streamClose(handStream);
handTrackerStop(tracker);
handTrackerRelease(tracker);

sensorDisconnect(sensor);

releaseSensorContext(context);