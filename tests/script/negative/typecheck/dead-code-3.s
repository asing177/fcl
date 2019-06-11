@initial
init() {
  transitionTo(@a);
}

@a
loop() {
  transitionTo(@a);
  terminate(); // never reachable
}
