global decimal<2> m = 0.01;
global decimal<3> n = 0.001;

@initial
init() {
  m = m * n; // bad
  transitionTo(@terminal)
}
