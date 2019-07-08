// Colors are either monochrome or defined by hue-saturation-brightness. All
// int values should be in the range 0-255.
type Color {
  Monochrome(int brightness);
  HSB(int hue, int saturation, int brightness);
}

global Color myColor = Monochrome(0);

@initial
go() {
  myColor = increaseBrightness(myColor);
  terminate()
}

// Calculate the average brightness of two colors.
averageBrightness(Color c1, Color c2) {
  (c1.brightness + c2.brightness) / 2
}

// Increment a color's brightness by 1 if that color is not yet maximally bright.
increaseBrightness(Color c) {
  if (c.brightness <= 255) {
    c.brightness = c.brightness + 1;
  };
  c
}
