decimal<2> m1;
decimal<2> m2;
decimal<2> m3;
decimal<3> n = 0.125;

@initial
init() {
  m1 = round(2,n);     // expect m  = 0.13
  m2 = roundDown(2,n); // expect m2 = 0.12
  m3 = roundUp(2,n);   // expect m3 = 0.13
  transitionTo(@terminal)
}
