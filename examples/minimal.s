global int x = 0 ;

transition initial -> set;
transition set -> terminal;

@set
end () {
  terminate();
}

@initial
setX () {
  x = 42;
  transitionTo(@set);
}
