decimal<2> m = 0.12;
decimal<3> n = 0.123;
decimal<5> x;

@initial
init() {
  x = m * n;
  // assert x == 0.01476;
  n = round(3, m * n);
  // assert n == 0.015;
  transitionTo(@terminal)
}
