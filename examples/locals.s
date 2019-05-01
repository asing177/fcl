local float x = 0.1;

transition initial -> set;
transition set -> terminal;

@set
end () {
  terminate();
}

@initial
setX () {
  x = x * 3.0;
  transitionTo(@set);
}
