int x;
int y;
int z;

@initial
f() {
  z = case x { // casing on undefined x
    0 -> y; // returning undefined y
    _ -> 1;
  };
  terminate()
}