// Code by David Pape 01634454

void setup() {
  // initialize digital pin LED_BUILTIN as an output.
  pinMode(LED_BUILTIN, OUTPUT);
  Serial.begin(115200);
}

void loop() {
  int sensorValue = analogRead(A0);
  sensorValue = map(sensorValue, 0, 1000, 0, 128);
  delay(50);                       // wait for 50ms: send updates 20x per second
  Serial.println(sensorValue);
}
