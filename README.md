# WiFi-and-Bluetooth-Travel-Time-Weighting
A weigthing function to detrin the travel time mode from Bluetooth and wifi connections.

Basically it takes the detection and unique id for each device for 5 sensors and finds the same device between different sensors.
With the timestamp at each sensor, we can calculate the travel time for each device.
Then, we weigh those travel times to estimate which mode is used for each device and distribute the travel time ofr each mode.
