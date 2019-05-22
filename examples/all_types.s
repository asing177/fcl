transition initial -> set;
transition set -> terminal;

@initial
manyTypes (
    int a
  , num b
  , text c
  , account d
  , asset<bool> e1
  , asset<int> e2
  , asset<decimal<1>> e3
  , contract f
  , sig g
  , datetime h
  , void i
) {
  transitionTo(@set);
}

@set
nothing () {
  terminate();
}
