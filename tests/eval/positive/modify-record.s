// colors are either monochrome or defined by hue-saturation-brightness. All
// int values should be in the range 0-255.
type color {
  Monochrome(int brightness);
  HSB(int hue, int saturation, int brightness);
}

global color mycolor = Monochrome(0);

@initial
go() {
  mycolor = increaseBrightness(mycolor);
  transitionTo(@initial)
}

@initial
end() {
  terminate()
}

// Calculate the average brightness of two colors.
averageBrightness(color c1, color c2) {
  (c1.brightness + c2.brightness) / 2
}

// Increment a color's brightness by 1 if that color is not yet maximally bright.
increaseBrightness(color c) {
  if (c.brightness <= 255) {
    c.brightness = c.brightness + 1;
  };
  c
}
