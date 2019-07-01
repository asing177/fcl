global int x = 0 ;

transition initial -> a;
transition a -> terminal;

@a
end () {
  terminate();
}

@initial
setX (int y) {
  x = 42 + y;
  transitionTo(@a);
}
