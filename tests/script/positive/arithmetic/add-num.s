decimal<2> m = 0.01;
num      n = 1/3;

@initial
init() {
  n = m + n; // expect n = 103/300
  transitionTo(@terminal)
}
