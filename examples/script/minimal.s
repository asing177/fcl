global int x = 0 ;

transition initial -> a;
transition a -> terminal;

@a
end () {
  terminate();
}

@initial
setX () {
  x = 42;
  transitionTo(@a);
}
