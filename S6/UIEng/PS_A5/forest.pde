// Code by David Pape 01634454
// adapted from Dan Shiffman's Tree example

import processing.serial.*;

Serial myPort;
String val;
int[] recentVals = new int[10];
int i;
int intVal;
float theta;   

void setup() {
   size(640, 480);
   print(Serial.list());
   myPort = new Serial(this, Serial.list()[2], 115200);
}

void draw() {
  if ( myPort.available() > 0) {
    val = myPort.readStringUntil('\n');
  }
  
  // Moisture data is saved in an array
  // the last 10 datapoints are averaged so the trees are moving smoothly
  if (val != null) {
    recentVals[i++ % recentVals.length] = Integer.parseInt(val.replaceAll("\\s+",""));
    float sum = 0;
    for (int i = 0; i < recentVals.length; i++)
      sum += recentVals[i];
    intVal = (int) (sum / recentVals.length);
    
    background(32, 97, 35);
    frameRate(30);
    
    text(val, 2, 20);
    
    tree(220, 160, 0.66, 0.75);
    tree(280, 50, 0.6, 0.4);
    tree(330, 35, 0.6, 0.4);
    tree(540, 190, 0.6, 0.4);
    tree(410, 90,  0.65, 1);
    tree(470, 35, 0.7, 0.6);
    tree(30, 30, 0.6, 0.4);
    tree(80, 70,  0.70, 1);
    tree(160, 40, 0.6, 0.4);
  }
}

void tree(int x, int stemHeight, float densityFactor, float spreadFactor) {
  // Let's pick an angle 0 to 90 degrees based on the mouse position
  float a = (spreadFactor * intVal / (float) 255) * 90f;
  // Convert it to radians
  theta = radians(a);
  // Start the tree from the bottom of the screen
  pushMatrix();
  translate(x,height);
  // Draw a line 120 pixels
  stroke(84, 57, 24);
  strokeWeight(3 * (float) Math.log(Math.abs(stemHeight)));
  line(0,0,0,-stemHeight);
  // Move to the end of that line
  translate(0,-stemHeight);
  // Start the recursive branching!
  
  branch(stemHeight, densityFactor);
  
  popMatrix();
}


void branch(float h, float densityFactor) {
  // Each branch will be 2/3rds the size of the previous one
  h *= densityFactor;
  
  // All recursive functions must have an exit condition!!!!
  // Here, ours is when the length of the branch is 2 pixels or less
  if (h > 2) {
    
    // Set different colors for tree trunk or branches
    if (h < 40) {
      stroke(38, 212, 47);
      strokeWeight((float) Math.log(Math.abs(h)));
    } else {
       stroke(84, 57, 24);
      strokeWeight(2 * (float) Math.log(Math.abs(h)));
     }
    
    pushMatrix();    // Save the current state of transformation (i.e. where are we now)
    rotate(theta);   // Rotate by theta
    line(0, 0, 0, -h);  // Draw the branch
    translate(0, -h); // Move to the end of the branch
    branch(h, densityFactor);       // Ok, now call myself to draw two new branches!!
    popMatrix();     // Whenever we get back here, we "pop" in order to restore the previous matrix state
    
    // Set different colors for tree trunk or branches
    if (h < 40) {
      stroke(38, 212, 47);
      strokeWeight((float) Math.log(Math.abs(h)));
    } else {
      stroke(84, 57, 24);
      strokeWeight(2 * (float) Math.log(Math.abs(h)));
    }
 
    // Repeat the same thing, only branch off to the "left" this time!
    pushMatrix();
    rotate(-theta);
    line(0, 0, 0, -h);
    translate(0, -h);
    branch(h, densityFactor);
    popMatrix();
  }
}
