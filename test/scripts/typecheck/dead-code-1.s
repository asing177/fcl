// Example of (dead) code after a transition.

transition initial -> terminal;

@initial
f () {
  terminate();
  a = 1;
}
