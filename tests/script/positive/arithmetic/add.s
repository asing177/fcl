decimal<2> m = 0.01;
decimal<3> n = 0.001;

@initial
init() {
  n = m + n; // expect n = 0.011
  transitionTo(@terminal)
}
