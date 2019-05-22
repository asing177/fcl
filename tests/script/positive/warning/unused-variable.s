global int alwaysUsed;
global int sometimesUsed;
global int neverUsed; // Warn only about this one

transition initial -> a;
transition a -> terminal;

@initial
init1() {
  alwaysUsed = 1;
  transitionTo(@a);
}

@initial
init2() {
  alwaysUsed = 2;
  sometimesUsed = 3;
  transitionTo(@a);
}

@a
end() {
  terminate();
}
