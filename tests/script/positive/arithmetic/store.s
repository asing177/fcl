// It should be ok to store a lower precision value in a higher
// precision variable; this is analogous to adding with a higher
// precision 0, as we do in the case of n2 below.

global decimal<1> m = 0.1;
global decimal<2> n1;
global decimal<2> n2;

@initial
init() {
  n1 = m;        // expect n1 = 0.10
  n2 = m + 0.00; // expect n2 = 0.10
  transitionTo(@terminal)
}
