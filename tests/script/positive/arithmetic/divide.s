global decimal<1> m = 0.1;
global decimal<2> n = 0.12;
global num        x;

@initial
init() {
  x = m / n;
  // assert x == 5/6;
  m = round(1, x);
  // assert m = 0.8;
  transitionTo(@terminal)
}
