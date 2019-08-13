# Changelog for fcl

## 0.2


* Removed "soundness checking" for safe workflows, implemented safe workflow recognition algorithm (split-and-merge analysis)
* More property-based tests for soundness checking and split-and-merge analysis
* Generalisation of graphviz generation algorithm
* Extend `enum` declarations to full variant record declarations with typesafe
  record access and assignment. Below is an example which shows the
  declaration of a type `color` with constructors `HSB` and `Monochrome`. The
  constructors are parameterised by fields with a type and a parameter name.
  When a field is declared for all constructors, then it can be used for field
  access using the dot (`.`) operator.

  ~~~
  // Colors are either monochrome or defined by hue-saturation-brightness. All
  // int values should be in the range 0-255.
  type color {
    Monochrome(int brightness);
    HSB(int hue, int saturation, int brightness);
  }

  // Calculate the average brightness of two colors.
  averageBrightness(color c1, color c2) {
    (c1.brightness + c2.brightness) / 2
  }
  ~~~
  Notes:
    - The `enum` and new `type` keywords are interchangeable.
    - `case` statements are no longer guaranteed to be exhaustive, thus a
      pattern match failure at runtime is possible.
* Generate Swagger specification for a Servant API
* Use `ObjectWithSingleField` constructor for encoding/decoding sum datatypes to/from JSON
* Add information of the calling method in deltas when transferring assets
* Remove SafeInteger and SafeString modules
* Constrain workflows to be free choice Petri nets, hence making soundness checking more efficient

## 0.1

* Initial release.
