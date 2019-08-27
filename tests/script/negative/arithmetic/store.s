global decimal<2> m = 0.11;
global decimal<1> n;

@initial
init() {
  n = m;        // bad
  n = m + 0.00; // bad
  n = 0.00;     // bad
  transitionTo(@terminal)
}
