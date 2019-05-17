// Subtraction should work the same as addition

decimal<2> m = 0.01;
decimal<3> n = 0.001;
decimal<3> x;
decimal<3> y;

@initial
init() {
  x = m - n;
  // assert x == 0.009;
  y = n - m;
  // assert y == -0.009;
  transitionTo(@terminal)
}
