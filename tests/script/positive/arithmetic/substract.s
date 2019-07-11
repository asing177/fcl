// Subtraction should work the same as addition

global decimal<2> m = 0.01;
global decimal<3> n = 0.001;
global decimal<3> x;
global decimal<3> y;

@initial
init() {
  x = m - n;
  // assert x == 0.009;
  y = n - m;
  // assert y == -0.009;
  transitionTo(@terminal)
}
