# Changelog for fcl

## 0.2

* Extend `enum` declarations to full variant record declarations. The `enum`
  and `type` Allow
  typesafe record access and assignment. Below is an example which shows the declaration of a type `Color` with constructors `HSB` and `Monochrome`. The
  constructors are parameterised by fields with a type and a parameter name.
  When a field is declared for all constructors, then it can be used for field
  access.
  ~~~
  // Colors are either monochrome or defined by hue-saturation-brightness. All
  // int values should be in the range 0-255.
  type Color {
    Monochrome(int brightness);
    HSB(int hue, int saturation, int brightness);
  }

  // Calculate the average brightness of two colors.
  averageBrightness(Color c1, Color c2) {
    (c1.brightness + c2.brightness) / 2
  }
  ~~~


## 0.1

* Initial release.
