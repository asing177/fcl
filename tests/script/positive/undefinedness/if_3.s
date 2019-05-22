global int a;
global int b;

transition initial -> myInitial;
transition myInitial -> stateA;
transition myInitial -> stateB;
transition stateA -> terminal;
transition stateB -> terminal;

@initial
go() {
  transitionTo(:myInitial);
}

@myInitial
init() {
  if (sender() == deployer()) {
     a = 10;
     transitionTo(:stateA);
  } else {
    transitionTo(:stateB);
  };
}

@stateA
exitOne() {
  b = a + 10;
  terminate();
}

@stateB
exitTwo() {
  b = 20 + 20;
  terminate();
}
