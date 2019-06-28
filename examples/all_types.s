transition initial -> a;
transition a -> terminal;

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
) {
  transitionTo(@a);
}

@a
nothing () {
  terminate();
}
