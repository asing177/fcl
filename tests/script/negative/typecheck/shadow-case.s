int x;

@initial
f() {
  y = case x {
    x -> x; // shadowing global x
  };
  terminate()
}

// helper
h(int y) {
  z = case y {
    y -> y; // shadowing parameter y
  }
}
